{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, RecordWildCards
  , TemplateHaskell, TypeFamilies, OverloadedStrings #-}

module Main where
import Control.Applicative  ((<$>), optional)
import Control.Exception    (bracket)
import Control.Monad        (msum, mzero)
import Control.Monad.Reader (ask)
import Control.Monad.State  (get, put)
import Control.Monad.Trans  (liftIO)
import Data.Acid            (AcidState, Update, Query, makeAcidic, openLocalState)
import Data.Acid.Advanced   (update', query')
import Data.Acid.Local      (createCheckpointAndClose)
import Data.Data            (Data, Typeable)
import Data.IxSet           ( Indexable(..), IxSet(..), (@=), Proxy(..), getOne
                            , ixFun, ixSet )
import qualified Data.IxSet as IxSet
import Data.SafeCopy        (SafeCopy, base, deriveSafeCopy)
import Data.Text            (Text)
import Data.Text.Lazy       (toStrict)
import qualified Data.Text  as Text
import Data.Time            (UTCTime(..), getCurrentTime)
import Happstack.Server     ( ServerPart, Method(POST, HEAD, GET), Response, decodeBody
                            , defaultBodyPolicy, dir, lookRead, lookText, method
                            , notFound, nullConf, nullDir, ok, seeOther, simpleHTTP
                            , toResponse)
import qualified Text.Blaze.Html4.Strict as H
import qualified Text.Blaze.Html4.Strict.Attributes as A
import SubjectView

newtype SubjectId = SubjectId { unSubjectId :: Integer }
    deriving (Eq, Ord, Data, Enum, Typeable, SafeCopy)
newtype SubjectName     = SubjectName Text    deriving (Eq, Ord, Data, Typeable, SafeCopy)

data Subject = Subject
    { subjectID  :: SubjectId
    , subjectName  :: Text
    }
    deriving (Eq, Ord, Data, Typeable)

$(deriveSafeCopy 0 'base ''Subject)
instance Indexable Subject where
    empty = ixSet [ ixFun $ \bp -> [ subjectID bp ]
                  , ixFun $ \bp -> [ SubjectName  $ subjectName bp  ]
                  ]
data PlannerState = PlannerState
    { nextSubjectId :: SubjectId
    , subjects      :: IxSet Subject
    }
    deriving (Eq, Ord,Data, Typeable)

$(deriveSafeCopy 0 'base ''PlannerState)

initialPlannerState :: PlannerState
initialPlannerState =
            PlannerState { nextSubjectId = SubjectId 1
                       , subjects      = empty
                       }

-- | create a new, empty post and add it to the database
newSubject :: Text -> Update PlannerState Subject
newSubject subjectName =
    do b@PlannerState{..} <- get
       let subject = Subject { subjectID = nextSubjectId
                       , subjectName  = Text.empty
                                 }
       put $ b { nextSubjectId = succ nextSubjectId
               , subjects      = IxSet.insert subject subjects
               }
       return subject

main :: IO ()
main =
    do bracket (openLocalState initialPlannerState)
               (createCheckpointAndClose)
               (\dispatcher ->
                    simpleHTTP nullConf (handlers dispatcher))

handlers :: AcidState PlannerState -> ServerPart Response
handlers dispatcher =
    msum [ dir "subject" $ subjectDispatcher
    	 , do nullDir
	      ok $ toResponse $ "New count is: "
         ]

