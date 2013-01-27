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
                            , defaultBodyPolicy, dir, dirs, lookRead, lookText, method
                            , notFound, nullConf, nullDir, ok, seeOther, simpleHTTP
                            , toResponse)
import Text.Blaze.Html5 (Html, (!), a, form, input, p, toHtml, label)
import Text.Blaze.Html5.Attributes (action, enctype, href, name, size, type_, value)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Template
import Model
import SubjectView
import GroupView
import RoomView
import SlotView
import PlanView
import SettingsView
import EntryView


mainPage :: AcidState Planner -> ServerPart Response
mainPage acid =
    do  ok $ template "Strona główna" [] $ do "Strona główna - wybierz coś na górze"


route :: AcidState Planner -> ServerPart Response
route acid =
    do decodeBody (defaultBodyPolicy "/tmp/" 0 1000000 1000000)
       msum [ dirs "favicon.ico"    $ notFound (toResponse ())
            , dirs "subjects/edit"  $ editSubjectView acid
            , dirs "subjects/new"    $ newSubjectView acid
            , dirs "subjects/view"  $ viewSubject acid
            , dirs "subjects/infoHours"   $ subjectInfoHours acid
            , dirs "subjects/info"   $ subjectInfo acid
            , dirs "subjects"        $ showSubjects acid
            , dirs "groups/edit"  $ editGroupView acid
            , dirs "groups/new"    $ newGroupView acid
            , dirs "groups/view"  $ viewGroup acid
            , dirs "groups/info"   $ groupInfo acid	
            , dirs "groups"        $ showGroups acid
            , dirs "rooms/edit"  $ editRoomView acid
            , dirs "rooms/new"    $ newRoomView acid
            , dirs "rooms/view"  $ viewRoom acid
            , dirs "rooms/info"   $ roomInfo acid			
            , dirs "rooms"        $ showRooms acid
            , dirs "slots/edit"  $ editSlotView acid
            , dirs "slots/new"    $ newSlotView acid
            , dirs "slots/view"  $ viewSlot acid
            , dirs "slots/info"   $ slotInfo acid
            , dirs "slots"        $ showSlots acid
			, dirs "entries/edit"  $ editEntryView acid
            , dirs "entries/new"    $ newEntryView acid
            , dirs "entries/view"  $ viewEntry acid
			, dirs "entries"        $ showEntries acid
			, dirs "planTable"      $ planTable acid
			, dirs "auto"      $ automaticPlan acid
			, dirs "settings"	$ viewSettings acid
            , nullDir               >> mainPage acid
            ]


-- | start acid-state and the http server
main :: IO ()
main =
    do bracket (openLocalState initialPlannerState)
               (createCheckpointAndClose)
               (\acid ->
                    simpleHTTP nullConf (route acid))