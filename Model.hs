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
newtype SubjectName     = SubjectName Text    deriving (Eq, Ord, Data, Typeable, SafeCopy, Show)
newtype SubjectDesc     = SubjectDesc Text    deriving (Eq, Ord, Data, Typeable, SafeCopy)
newtype HoursPerWeek     = HoursPerWeek Int    deriving (Eq, Ord, Data, Typeable, SafeCopy)
data Subject = Subject
    { subjectId  :: SubjectId
    , subjectName   :: Text
    , subjectDesc ::   Text
    , hoursPerWeek :: Int
    }
    deriving (Eq, Ord, Data, Typeable)
$(deriveSafeCopy 0 'base ''Subject)
instance Indexable Subject where
    empty = ixSet [ ixFun $ \bp -> [ subjectId bp ]
                  , ixFun $ \bp -> [ SubjectName  $ subjectName bp  ]
                  , ixFun $ \bp -> [ SubjectDesc  $ subjectDesc bp  ]
                  , ixFun $ \bp -> [ HoursPerWeek  $ hoursPerWeek bp  ]
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
---------Slot czasowy w planie
-------------------------------------------------------
newtype SlotId = SlotId { unSlotId :: Integer }
   deriving (Eq, Ord, Data, Enum, Typeable, SafeCopy)
data Slot = Slot
   { slotId  :: SlotId
   , slotTime   :: Text
   }
   deriving (Eq, Ord, Data, Typeable)
$(deriveSafeCopy 0 'base ''Slot)
newtype SlotTime     = SlotTime Text    deriving (Eq, Ord, Data, Typeable, SafeCopy)
instance Indexable Slot where
   empty = ixSet [ ixFun $ \bp -> [ slotId bp ]
                 , ixFun $ \bp -> [ SlotTime  $ slotTime bp  ]
                 ]
-------------------------------------------------------
---------Entry
-------------------------------------------------------
newtype EntryId = EntryId { unEntryId :: Integer }
   deriving (Eq, Ord, Data, Enum, Typeable, SafeCopy)
data Entry = Entry
   { entryId  :: EntryId
   , subjectIdFk   :: Integer
   , groupIdFk   :: Integer
   , roomIdFk   :: Integer
   , slotIdFk   :: Integer
   }
   deriving (Eq, Ord, Data, Typeable)
$(deriveSafeCopy 0 'base ''Entry)
newtype SubjectIdFk    = SubjectIdFk Int    deriving (Eq, Ord, Data, Typeable, SafeCopy)
newtype GroupIdFk     = GroupIdFk Int    deriving (Eq, Ord, Data, Typeable, SafeCopy)
newtype RoomIdFk    = RoomIdFk Int    deriving (Eq, Ord, Data, Typeable, SafeCopy)
newtype SlotIdFk     = SlotIdFk Int    deriving (Eq, Ord, Data, Typeable, SafeCopy)

instance Indexable Entry where
   empty = ixSet [ ixFun $ \bp -> [ entryId bp ]
                 , ixFun $ \bp -> [ SubjectId  $ subjectIdFk bp  ]
                 , ixFun $ \bp -> [ GroupId  $ groupIdFk bp  ]
                 , ixFun $ \bp -> [ RoomId  $ roomIdFk bp  ]
                 , ixFun $ \bp -> [ SlotId  $ slotIdFk bp  ]
                 ]
-------------------------------------------------------
---------Settings
-------------------------------------------------------
data Settings = Settings { maxDailyHoursPerGroup :: Int
							, startTime :: Int
							, endTime :: Int
							}
							deriving (Eq, Ord, Data, Typeable)
$(deriveSafeCopy 0 'base ''Settings)

updateSettings :: Settings -> Update Planner ()
updateSettings updatedSettings =
    do b@Planner{..} <- get
       put $ b { settings = updatedSettings
               }
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
    , nextSlotId :: SlotId
    , slots      :: IxSet Slot
    , nextEntryId :: EntryId
    , entrys      :: IxSet Entry
	, settings	:: Settings
    }
    deriving (Data, Typeable)

$(deriveSafeCopy 0 'base ''Planner)

peekSettings :: Query Planner Settings
peekSettings = settings <$> ask


----------------------------------------------- --------
---------Subject  functions
-------------------------------------------------------
-- | create a new, empty subject and add it to the database
newSubject :: UTCTime -> Update Planner Subject
newSubject pubDate =
    do b@Planner{..} <- get
       let subject = Subject { subjectId = nextSubjectId
                       , subjectName  = Text.empty
                       , subjectDesc  = Text.empty
                       , hoursPerWeek = 0
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
---------Slot  functions
-------------------------------------------------------
-- | create a new, empty slot and add it to the database
newSlot :: UTCTime -> Update Planner Slot
newSlot pubDate =
    do b@Planner{..} <- get
       let slot = Slot { slotId = nextSlotId
                       , slotTime  =  Text.empty
                       }
       put $ b { nextSlotId = succ nextSlotId
               , slots      = IxSet.insert slot slots
               }
       return slot
-- | update the slot in the database (indexed by SlotId)
updateSlot :: Slot -> Update Planner ()
updateSlot updatedSlot =
    do b@Planner{..} <- get
       put $ b { slots = IxSet.updateIx (slotId updatedSlot) updatedSlot slots
               }
slotById :: SlotId -> Query Planner (Maybe Slot)
slotById pid =
     do Planner{..} <- ask
        return $ getOne $ slots @= pid
slotsAll ::  Query Planner [Slot]
slotsAll  =
    do Planner{..} <- ask
       return $ IxSet.toList  $ slots

-------------------------------------------------------
---------Entry  functions
-------------------------------------------------------
-- | create a new, empty entry and add it to the database
newEntry :: UTCTime -> Update Planner Entry
newEntry pubDate =
    do b@Planner{..} <- get
       let entry = Entry { entryId = nextEntryId
                       , subjectIdFk   = 0
                       , groupIdFk   = 0
                       , roomIdFk   = 0
                       , slotIdFk   = 0
                       }
       put $ b { nextEntryId = succ nextEntryId
               , entrys      = IxSet.insert entry entrys
               }
       return entry
-- | update the entry in the database (indexed by EntryId)
updateEntry :: Entry -> Update Planner ()
updateEntry updatedEntry =
    do b@Planner{..} <- get
       put $ b { entrys = IxSet.updateIx (entryId updatedEntry) updatedEntry entrys
               }
entryById :: EntryId -> Query Planner (Maybe Entry)
entryById pid =
     do Planner{..} <- ask
        return $ getOne $ entrys @= pid
entriesAll ::  Query Planner [Entry]
entriesAll  =
    do Planner{..} <- ask
       return $ IxSet.toList  $ entrys


initialPlannerState :: Planner
initialPlannerState =
 Planner { nextSubjectId = SubjectId 1
      , subjects      = empty
      , nextGroupId = GroupId 1
      , groups      = empty
      , nextRoomId = RoomId 1
      , rooms      = empty
      , nextSlotId = SlotId 1
      , slots      = empty
      , nextEntryId = EntryId 1
      , entrys      = empty
	  , settings 	= Settings 6 8 16		-- default settings
      }

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
  , 'newSlot
  , 'updateSlot
  , 'slotById
  , 'slotsAll
  , 'newEntry
  , 'updateEntry
  , 'entryById
  , 'entriesAll
  , 'peekSettings
  , 'updateSettings
  ])
 