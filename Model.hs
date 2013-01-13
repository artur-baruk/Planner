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
---------Group
-------------------------------------------------------
newtype GroupId = GroupId { unGroupId :: Integer }
    deriving (Eq, Ord, Data, Enum, Typeable, SafeCopy)
data Group = Group
    { groupId  :: GroupId
    , groupName   :: Text
    }
    deriving (Eq, Ord, Data, Typeable)
$(deriveSafeCopy 0 'base ''Group)
newtype GroupName     = GroupName Text    deriving (Eq, Ord, Data, Typeable, SafeCopy)
instance Indexable Group where
    empty = ixSet [ ixFun $ \bp -> [ groupId bp ]
                  , ixFun $ \bp -> [ GroupName  $ groupName bp  ]
                  ]
-------------------------------------------------------
---------Room
-------------------------------------------------------
newtype RoomId = RoomId { unRoomId :: Integer }
    deriving (Eq, Ord, Data, Enum, Typeable, SafeCopy)
data Room = Room
    { roomId  :: RoomId
    , roomName   :: Text
    }
    deriving (Eq, Ord, Data, Typeable)
$(deriveSafeCopy 0 'base ''Room)
newtype RoomName     = RoomName Text    deriving (Eq, Ord, Data, Typeable, SafeCopy)
instance Indexable Room where
    empty = ixSet [ ixFun $ \bp -> [ roomId bp ]
                  , ixFun $ \bp -> [ RoomName  $ roomName bp  ]
                  ]
-------------------------------------------------------
---------Planner
-------------------------------------------------------
data Planner = Planner
    { nextSubjectId :: SubjectId
    , subjects      :: IxSet Subject
    , nextGroupId :: GroupId
    , groups      :: IxSet Group
    , nextRoomId :: RoomId
    , rooms      :: IxSet Room
    }
    deriving (Data, Typeable)

$(deriveSafeCopy 0 'base ''Planner)

initialPlannerState :: Planner
initialPlannerState =
    Planner { nextSubjectId = SubjectId 1
         , subjects      = empty
         , nextGroupId = GroupId 1
         , groups      = empty
         , nextRoomId = RoomId 1
         , rooms      = empty
         }

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

-------------------------------------------------------
---------Group  functions
-------------------------------------------------------
-- | create a new, empty group and add it to the database
newGroup :: UTCTime -> Update Planner Group
newGroup pubDate =
    do b@Planner{..} <- get
       let group = Group { groupId = nextGroupId
                       , groupName  = Text.empty
                       }
       put $ b { nextGroupId = succ nextGroupId
               , groups      = IxSet.insert group groups
               }
       return group
-- | update the group in the database (indexed by GroupId)
updateGroup :: Group -> Update Planner ()
updateGroup updatedGroup =
    do b@Planner{..} <- get
       put $ b { groups = IxSet.updateIx (groupId updatedGroup) updatedGroup groups
               }
groupById :: GroupId -> Query Planner (Maybe Group)
groupById pid =
     do Planner{..} <- ask
        return $ getOne $ groups @= pid
groupsAll ::  Query Planner [Group]
groupsAll  =
    do Planner{..} <- ask
       return $ IxSet.toList  $ groups


-------------------------------------------------------
---------Room  functions
-------------------------------------------------------
-- | create a new, empty room and add it to the database
newRoom :: UTCTime -> Update Planner Room
newRoom pubDate =
    do b@Planner{..} <- get
       let room = Room { roomId = nextRoomId
                       , roomName  = Text.empty
                       }
       put $ b { nextRoomId = succ nextRoomId
               , rooms      = IxSet.insert room rooms
               }
       return room
-- | update the room in the database (indexed by RoomId)
updateRoom :: Room -> Update Planner ()
updateRoom updatedRoom =
    do b@Planner{..} <- get
       put $ b { rooms = IxSet.updateIx (roomId updatedRoom) updatedRoom rooms
               }
roomById :: RoomId -> Query Planner (Maybe Room)
roomById pid =
     do Planner{..} <- ask
        return $ getOne $ rooms @= pid
roomsAll ::  Query Planner [Room]
roomsAll  =
    do Planner{..} <- ask
       return $ IxSet.toList  $ rooms







-------------------------------------------------------
---------global  acidic
-------------------------------------------------------
$(makeAcidic ''Planner
  [ 'newSubject
  , 'updateSubject
  , 'subjectById
  , 'subjectsAll
  , 'newGroup
  , 'updateGroup
  , 'groupById
  , 'groupsAll
  , 'newRoom
  , 'updateRoom
  , 'roomById
  , 'roomsAll
  ])
