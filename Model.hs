{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving, 
   MultiParamTypeClasses, TemplateHaskell, TypeFamilies, RecordWildCards #-}

module Model where
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

-------------------------------------------------------
---------Subject
-------------------------------------------------------
newtype SubjectId = SubjectId { unSubjectId :: Integer }
    deriving (Eq, Ord, Data, Enum, Typeable, SafeCopy)
data Subject = Subject
    { subjectId  :: SubjectId
    , subjectName   :: Text
    }
    deriving (Eq, Ord, Data, Typeable)
$(deriveSafeCopy 0 'base ''Subject)
newtype SubjectName     = SubjectName Text    deriving (Eq, Ord, Data, Typeable, SafeCopy)
instance Indexable Subject where
    empty = ixSet [ ixFun $ \bp -> [ subjectId bp ]
                  , ixFun $ \bp -> [ SubjectName  $ subjectName bp  ]
                  ]
-------------------------------------------------------
---------Planner
-------------------------------------------------------
data Planner = Planner
    { nextSubjectId :: SubjectId
    , subjects      :: IxSet Subject
    }
    deriving (Data, Typeable)

$(deriveSafeCopy 0 'base ''Planner)

initialPlannerState :: Planner
initialPlannerState =
    Planner { nextSubjectId = SubjectId 1
         , subjects      = empty   }

-------------------------------------------------------
---------Subject  functions
-------------------------------------------------------
-- | create a new, empty subject and add it to the database
newSubject :: UTCTime -> Update Planner Subject
newSubject pubDate =
    do b@Planner{..} <- get
       let subject = Subject { subjectId = nextSubjectId
                       , subjectName  = Text.empty
                       }
       put $ b { nextSubjectId = succ nextSubjectId
               , subjects      = IxSet.insert subject subjects
               }
       return subject
-- | update the subject in the database (indexed by SubjectId)
updateSubject :: Subject -> Update Planner ()
updateSubject updatedSubject =
    do b@Planner{..} <- get
       put $ b { subjects = IxSet.updateIx (subjectId updatedSubject) updatedSubject subjects
               }
subjectById :: SubjectId -> Query Planner (Maybe Subject)
subjectById pid =
     do Planner{..} <- ask
        return $ getOne $ subjects @= pid
subjectsAll ::  Query Planner [Subject]
subjectsAll  =
    do Planner{..} <- ask
       return $ IxSet.toList  $ subjects
$(makeAcidic ''Planner
  [ 'newSubject
  , 'updateSubject
  , 'subjectById
  , 'subjectsAll
  ])