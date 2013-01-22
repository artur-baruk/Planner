{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, RecordWildCards
  , TemplateHaskell, TypeFamilies, OverloadedStrings #-}
module EntryView where
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

editEntryView :: AcidState Planner -> ServerPart Response
editEntryView acid =
    do pid   <- EntryId <$> lookRead "id"
       mMsg  <- optional $ lookText "msg"
       mEntry <- query' acid (EntryById pid)
       allSubjects <- query' acid (SubjectsAll)
       allRooms <- query' acid (RoomsAll)
       allGroups <- query' acid (GroupsAll)
       allSlots <- query' acid (SlotsAll)
       case mEntry of
         Nothing ->
             notFound $ template "no such room" [] $ do "Nie Mozna odnalezc sali o ID: "
                                                        H.toHtml (unEntryId pid)
         (Just p@(Entry{..})) ->
             msum [ do method GET
                       ok $ template "foo" [] $ do
                         case mMsg of
                           (Just msg) | msg == "saved" -> "Zmiany zapisane!"
                           _ -> ""
                         H.form ! A.enctype "multipart/form-data"
                              ! A.method "POST"
                              ! A.action (H.toValue $ "/entries/edit?id=" ++
                                                      (show $ unEntryId pid)) $ do
                           H.label "Przedmiot" ! A.for "Nazwa"
                           H.select ! A.name "subject" $ mapM_ (subjectOption ) allSubjects
                           H.br
                           H.label "Sala" ! A.for "Nazwa"
                           H.select ! A.name "room" $ mapM_ (roomOption ) allRooms
                           H.br
                           H.label "Grupa" ! A.for "Nazwa"
                           H.select ! A.name "group" $ mapM_ (groupOption ) allGroups
                           H.br
                           H.label "Godzina" ! A.for "Nazwa"
                           H.select ! A.name "slot" $ mapM_ (slotOption ) allSlots
                           H.br
                           H.button ! A.name "status" ! A.value "Zapisz" $ "zapisz"
                  , do method POST
                       nr   <- lookRead "nr"
                       let updatedEntry =
                               p { subjectIdFk = nr
                                 }
                       update' acid (UpdateEntry updatedEntry)
                       seeOther ("/entries/view?id=" ++ (show $ unEntryId pid))
                                    (toResponse ())
                  ]

                 --where lookText' = fmap toStrict . lookText
-- | create a new planner room in the database , and then redirect to /edit
newEntryView :: AcidState Planner -> ServerPart Response
newEntryView acid =
    do method POST
       now <- liftIO $ getCurrentTime
       entry <- update' acid (NewEntry now)
       seeOther ("/entries/edit?id=" ++ show (unEntryId $ entryId entry)) (toResponse ())
-- | render a single planner room into an HTML fragment
entryHtml :: AcidState Planner -> Entry -> Html
entryHtml acid (Entry{..}) =
		H.div ! A.class_ "entry" $ do
			H.div ! A.class_ "subjectIdFk" $ do "subject: "    >> H.toHtml subjectIdFk
			H.div ! A.class_ "groupIdFk" $ do "group: "    >> H.toHtml groupIdFk
			H.div ! A.class_ "roomIdFk" $ do "room: "    >> H.toHtml roomIdFk
			H.div ! A.class_ "post-footer" $ do
				 H.span $ H.a ! A.href (H.toValue $ "/entries/view?id=" ++
										show (unEntryId entryId)) $ "Otworz"
				 H.span $ " "
				 H.span $ H.a ! A.href (H.toValue $ "/entries/edit?id=" ++
										show (unEntryId entryId)) $ "Edytuj wpis"
-- | view a single planner room
viewEntry :: AcidState Planner -> ServerPart Response
viewEntry acid =
    do pid <- EntryId <$> lookRead "id"
       mEntry <- query' acid (EntryById pid)
       case mEntry of
         Nothing ->
             notFound $ template "no such room" [] $ do "Could not find an entry with id "
                                                        H.toHtml (unEntryId pid)
         (Just p) ->
             ok $ template ("Entry") [] $ do
                 (entryHtml acid p)

showEntries :: AcidState Planner -> ServerPart Response
showEntries acid =
    do allEntries <- query' acid (EntriesAll)
       ok $ template "Lista wpisow" [] $ do
         mapM_ (entryHtml acid) allEntries


subjectOption  :: Subject -> Html
subjectOption (Subject{..}) =
  H.option ! H.customAttribute "value" (H.toValue (show (unSubjectId subjectId))) $ H.toHtml subjectName


roomOption  :: Room -> Html
roomOption (Room{..}) =
  H.option ! H.customAttribute "value" (H.toValue (show (unRoomId roomId))) $ H.toHtml roomName

groupOption  :: Group -> Html
groupOption (Group{..}) =
  H.option ! H.customAttribute "value" (H.toValue (show (unGroupId groupId))) $ H.toHtml groupName

slotOption  :: Slot -> Html
slotOption (Slot{..}) =
  H.option ! H.customAttribute "value" (H.toValue (show (unSlotId slotId))) $ H.toHtml slotTime




