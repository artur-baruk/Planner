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


newtype SubjectId = SubjectId { unSubjectId :: Integer }
    deriving (Eq, Ord, Data, Enum, Typeable, SafeCopy)
data Status =
    Draft
  | Published
    deriving (Eq, Ord, Data, Typeable)

$(deriveSafeCopy 0 'base ''Status)
data Subject = Subject
    { subjectId  :: SubjectId
    , title   :: Text
    , author  :: Text
    , body    :: Text
    , date    :: UTCTime
    , status  :: Status
    , tags    :: [Text]
    }
    deriving (Eq, Ord, Data, Typeable)

$(deriveSafeCopy 0 'base ''Subject)

newtype Title     = Title Text    deriving (Eq, Ord, Data, Typeable, SafeCopy)
newtype Author    = Author Text   deriving (Eq, Ord, Data, Typeable, SafeCopy)
newtype Tag       = Tag Text      deriving (Eq, Ord, Data, Typeable, SafeCopy)
newtype WordCount = WordCount Int deriving (Eq, Ord, Data, Typeable, SafeCopy)
instance Indexable Subject where
    empty = ixSet [ ixFun $ \bp -> [ subjectId bp ]
                  , ixFun $ \bp -> [ Title  $ title bp  ]
                  , ixFun $ \bp -> [ Author $ author bp ]
                  , ixFun $ \bp -> [ status bp ]
                  , ixFun $ \bp -> map Tag (tags bp)
                  , ixFun $ (:[]) . date  -- point-free, just for variety
                  , ixFun $ \bp -> [ WordCount (length $ Text.words $ body bp) ]
                  ]

data Planner = Planner
    { nextSubjectId :: SubjectId
    , subjects      :: IxSet Subject
    }
    deriving (Data, Typeable)

$(deriveSafeCopy 0 'base ''Planner)

initialPlannerState :: Planner
initialPlannerState =
    Planner { nextSubjectId = SubjectId 1
         , subjects      = empty
         }


-- | create a new, empty subject and add it to the database
newSubject :: UTCTime -> Update Planner Subject
newSubject pubDate =
    do b@Planner{..} <- get
       let subject = Subject { subjectId = nextSubjectId
                       , title  = Text.empty
                       , author = Text.empty
                       , body   = Text.empty
                       , date   = pubDate
                       , status = Draft
                       , tags   = []
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
subjectsByStatus :: Status -> Query Planner [Subject]
subjectsByStatus status =
    do Planner{..} <- ask
       return $ IxSet.toDescList (Proxy :: Proxy UTCTime) $ subjects @= status
$(makeAcidic ''Planner
  [ 'newSubject
  , 'updateSubject
  , 'subjectById
  , 'subjectsByStatus
  ])