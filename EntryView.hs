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
                            , defaultBodyPolicy, dir, dirs, look,lookReads, lookRead,lookText', lookText,lookInput, method
                            , notFound, nullConf, nullDir, ok, seeOther, simpleHTTP
                            , toResponse)
import Happstack.Server.RqData (RqData, look, getDataFn)
import Text.Blaze.Html5 (Html, (!), a, form, input, p, toHtml, label)
import Text.Blaze.Html5.Attributes (action, enctype, href, name, size, type_, value)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Template
import Model
import SubjectView


editEntryView :: AcidState Planner -> ServerPart Response
editEntryView acid =
    do pid   <- EntryId <$> lookRead "id"
       mMsg  <- optional $ lookText "msg"
       mEntry <- query' acid (EntryById pid)
       allEntries <- query' acid (EntriesByStatus Active)
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
                                      | msg /= "saved" -> H.toHtml msg
                           _ -> ""
                         H.form ! A.enctype "multipart/form-data"
                              ! A.method "POST"
                              ! A.action (H.toValue $ "/entries/edit?id=" ++
                                                      (show $ unEntryId pid)) $ do
                           H.br
                           H.label "Przedmiot" ! A.for "Nazwa"
                           H.select ! A.name "subject" ! A.class_  "subjectSelect" $ mapM_ (subjectOption ) allSubjects
                           H.input ! A.name "hoursPerWeek"
                                   ! A.id "hoursPerWeek"
                                   ! A.class_ "hoursPerWeek"
                                   ! A.type_ "number"
                                   ! A.style "display:none;"

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
                           H.button ! A.name "status" ! A.value "zapisz" $ "zapisz"
                           H.button ! A.name "status" ! A.value "usun" $ "usun"
                  , do method POST
                       subjectId   <- lookRead "subject"
                       hoursPerWeek <- lookRead "hoursPerWeek"
                       roomId   <- lookRead "room"
                       groupId   <- lookRead "group"
                       slotId   <- lookRead "slot"
                       stts  <- do s <- lookText "status"
                                   case s of
                                      "usun"    -> return NotActive
                                      "zapisz"    -> return Active
                                      _-> return Active
                       let oldEntryId  = pid
                       let msg = checkCond oldEntryId allEntries subjectId roomId  groupId  slotId hoursPerWeek
                       let msg2 = mapCond msg
                       let updatedEntry = if msg == "ok" 
											then  p { subjectIdFk = subjectId
													  , roomIdFk = roomId
													  , groupIdFk = groupId
													  , slotIdFk = slotId
													  , entryStatus = stts
													 }
											else  p { subjectIdFk = subjectId
													  , roomIdFk = roomId
													  , groupIdFk = groupId
													  , slotIdFk = slotId
													  , entryStatus = NotActive
													 }

                       update' acid (UpdateEntry updatedEntry)
                       case stts of
                         NotActive ->
                           seeOther ("/entries?ostatniousuniety=" ++ (show $ unEntryId pid) )
                                                               (toResponse ())
                         Active     -> if msg == "ok"  
												then seeOther ("/entries/view?id=" ++ (show $ unEntryId pid)++"&msg="++msg2 )
                                                              (toResponse ())
												else seeOther ("/entries/edit?id=" ++ (show $ unEntryId pid)++"&msg="++msg2 )
                                                              (toResponse ()) 
						]

checkCond :: EntryId -> [Entry] -> Integer ->Integer ->Integer ->Integer ->Integer ->String
checkCond oldEntryId allEntries subjectId roomId groupId slotId  hoursPerWeek =
       do  let hoursPerDay = 8
           if checkHoursSubject allEntries  hoursPerWeek subjectId groupId oldEntryId
                then "toManyHourSubject"
                else if 1==0  --checkHoursPerDay
                     then "toManyHoursPerDay"
                     else  if checkRoomBusy allEntries roomId  slotId oldEntryId
                                    then "roomBusy"
                                    else if checkGroupBusy allEntries groupId  slotId  oldEntryId
                                               then "groupBusy"
                                               else "ok"
mapCond string
     | string == "ok"         = "Zapis%20zakonczony%20powodzeniem"
     | string == "toManyHourSubject" = "Zapis%20zakonczony%20niepowodzeniem%20-przemiot%20ma%20za%20duzo%20godzin%20zajec%20w%20tygodniu"
     | string == "toManyHoursPerDay" = "Zapis%20zakonczony%20niepowodzeniem%20-grupa%20ma%20za%20duzo%20godzin%20zajec%20w%20ciagu%20dnia"
     | string == "roomBusy" =   "Zapis%20zakonczony%20niepowodzeniem%20-%20sala%20zajeta%20o%20wskazanej%20godzinie"
     | string == "groupBusy" =  "Zapis%20zakonczony%20niepowodzeniem%20-%20grupa%20zajeta%20o%20wskazanej%20godzinie"
     | otherwise              = "Zapis%20nieudany?"




--checkHoursSubject :: [Entry] -> Integer-> Integer-> Bool
checkHoursSubject allEntries hoursPerWeek subjectId groupId oldEntryId=
     if sum [checkHoursSubjectOne entry subjectId groupId oldEntryId |entry <-allEntries] >=  hoursPerWeek
                    then True
                    else False

--checkHoursSubjectOne :: Entry ->  Integer ->Integer
checkHoursSubjectOne (Entry{..})  subjectId groupId oldEntryId =   if     groupId == groupIdFk && subjectId == subjectIdFk  && oldEntryId  /=entryId
                                                                                                  then 1
                                                                                                  else 0



--checkGroupBusy :: [Entry] -> Integer -> Integer -> Bool
checkGroupBusy allEntries groupId slotId oldEntryId=
     orOper [checkGroupOne entry groupId slotId oldEntryId |entry <-allEntries]

--checkRoomBusy :: [Entry] -> Integer -> Integer -> Bool
checkRoomBusy allEntries roomId slotId oldEntryId=
     orOper [checkGroupOne entry roomId slotId oldEntryId |entry <-allEntries]

--jezeli true to warnuek jest spełniony i jest zle
--checkGroupOne :: Entry -> Integer -> Integer -> Bool
checkGroupOne (Entry{..}) groupId  slotId  oldEntryId= if     groupId == groupIdFk && slotId == slotIdFk  && oldEntryId  /=entryId
                                                    then True
                                                    else False
--checkRoomOne :: Entry -> Integer -> Integer -> Bool
checkRoomOne (Entry{..}) roomId  slotId  oldEntryId= if     roomId == roomIdFk && slotId == slotIdFk && oldEntryId  /=entryId
                                                    then True
                                                    else False
orOper :: [Bool]  -> Bool
orOper [] = False
orOper (x:xs) = orOper xs || x


--where lookText' = fmap toStrict . lookText
-- | create a new planner room in the database , and then redirect to /edit
newEntryView :: AcidState Planner -> ServerPart Response
newEntryView acid =
    do method POST
       now <- liftIO $ getCurrentTime
       entry <- update' acid (NewEntry now)
       seeOther ("/entries/edit?id=" ++ show (unEntryId $ entryId entry)) (toResponse ())
-- | render a single planner room into an HTML fragment
entryHtml :: Entry -> Html
entryHtml (Entry{..}) =
		H.div ! A.class_ "entry" $ do
			H.br
			H.b "Przedmiot: "
			H.i ! A.class_ "subjectGet" 
				  ! A.id (H.toValue subjectIdFk) $ do  "pusty przedmiot"
			H.br
			H.b "Grupa: "
			H.i ! A.class_ "groupGet"
				  ! A.id (H.toValue groupIdFk) $ do  "pusta grupa"
			H.br
			H.b "Sala: "
			H.i ! A.class_ "roomGet"
				  ! A.id (H.toValue roomIdFk) $ do  "pusty sala"
			H.br
			H.b "Godzina: "
			H.i ! A.class_ "slotGet" 
				  ! A.id (H.toValue slotIdFk) $ do  "pusty slot"
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
       mMsg  <- optional $ lookText "msg" 
       case mEntry of
         Nothing ->
             notFound $ template "no such room" [] $ do "Could not find an entry with id "
                                                        H.toHtml (unEntryId pid)
         (Just p) ->
             do      ok $ template ("Entry") [] $ do
                     case mMsg of
                           (Just msg) | msg == "saved" -> "Tak"
                                      | msg /= "saved" -> H.toHtml msg
                           Nothing -> ""
                     (entryHtml  p)

showEntries :: AcidState Planner -> ServerPart Response
showEntries acid =
    do allEntries <- query' acid (EntriesByStatus Active)
       ok $ template "Lista wpisow" [] $ do
         mapM_ entryHtml  allEntries


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


planTable :: AcidState Planner -> ServerPart Response
planTable acid = 
    do allEntries <- query' acid (EntriesByStatus Active)
       allGroups <- query' acid (GroupsAll)
       ok $ template "Tabelka z planem" [] $ do
            mapM_ (showGroup allEntries) allGroups

showGroup :: [Entry] -> Group  -> Html
showGroup allEntries (Group{..})  =
		H.div ! A.class_ "entry" $ do
			H.b  "Grupa: "
			H.b $ H.toHtml groupName
			H.table ! A.class_ "entryTab" $ do
				mapM_ (entryHtml2 (unGroupId groupId)) allEntries

entryHtml2 ::Integer -> Entry ->Html
entryHtml2 x (Entry{..})  =
	if  x == groupIdFk
		then	H.tr ! A.class_ "entry" $ do
					H.td ! A.class_ "slotGet"
						  ! A.id (H.toValue slotIdFk) $ do  "pusty slot"
					H.td ! A.class_ "roomGet"
						  ! A.id (H.toValue roomIdFk) $ do  "pusty sala"
					H.td ! A.class_ "subjectGet"
						  ! A.id (H.toValue subjectIdFk) $ do  "pusty przedmiot"
		else	""
		
--
--automaticPlan :: AcidState Planner -> ServerPart Response
--automaticPlan acid =
--    do pid   <- EntryId <$> lookRead "id"
--       mMsg  <- optional $ lookText "msg"
--       mEntry <- query' acid (EntryById pid)
--       allEntries <- query' acid (EntriesByStatus Active)
--       allSubjects <- query' acid (SubjectsAll)
--       allRooms <- query' acid (RoomsAll)
--       allGroups <- query' acid (GroupsAll)
--       allSlots <- query' acid (SlotsAll)
--       case mEntry of
--         Nothing ->
--             notFound $ template "no such room" [] $ do "Nie Mozna odnalezc sali o ID: "
--                                                        H.toHtml (unEntryId pid)
--         (Just p@(Entry{..})) ->
--             msum [ do method GET
--                       ok $ template "foo" [] $ do
--                         case mMsg of
--                           (Just msg) | msg == "saved" -> "Zmiany zapisane!"
--                                      | msg /= "saved" -> H.toHtml msg
--                           _ -> ""
--                         H.form ! A.enctype "multipart/form-data"
--                              ! A.method "POST"
--                              ! A.action (H.toValue $ "/entries/edit?id=" ++
--                                                      (show $ unEntryId pid)) $ do
--                           H.br
--                           H.label "Przedmiot" ! A.for "Nazwa"
--                           H.select ! A.name "subject" ! A.class_  "subjectSelect" $ mapM_ (subjectOption ) allSubjects
--                           H.input ! A.name "hoursPerWeek"
--                                   ! A.id "hoursPerWeek"
--                                   ! A.class_ "hoursPerWeek"
--                                   ! A.type_ "number"
--                                   ! A.style "display:none;"
--
--                           H.br
--                           H.label "Sala" ! A.for "Nazwa"
--                           H.select ! A.name "room" $ mapM_ (roomOption ) allRooms
--                           H.br
--                           H.label "Grupa" ! A.for "Nazwa"
--                           H.select ! A.name "group" $ mapM_ (groupOption ) allGroups
--                           H.br
--                           H.label "Godzina" ! A.for "Nazwa"
--                           H.select ! A.name "slot" $ mapM_ (slotOption ) allSlots
--                           H.br
--                           H.button ! A.name "status" ! A.value "zapisz" $ "zapisz"
--                           H.button ! A.name "status" ! A.value "usun" $ "usun"
--                  , do method POST
--                       subjectId   <- lookRead "subject"
--                       hoursPerWeek <- lookRead "hoursPerWeek"
--                       roomId   <- lookRead "room"
--                       groupId   <- lookRead "group"
--                       slotId   <- lookRead "slot"
--                       stts  <- do s <- lookText "status"
--                                   case s of
--                                      "usun"    -> return NotActive
--                                      "zapisz"    -> return Active
--                                      _-> return Active
--                       let oldEntryId  = pid
--                       let msg = checkCond oldEntryId allEntries subjectId roomId  groupId  slotId hoursPerWeek
--                       let msg2 = mapCond msg
--                       let updatedEntry = if msg == "ok"
--											then  p { subjectIdFk = subjectId
--													  , roomIdFk = roomId
--													  , groupIdFk = groupId
--													  , slotIdFk = slotId
--													  , entryStatus = stts
--													 }
--											else  p { subjectIdFk = subjectId
--													  , roomIdFk = roomId
--													  , groupIdFk = groupId
--													  , slotIdFk = slotId
--													  , entryStatus = NotActive
--													 }
--
--                       update' acid (UpdateEntry updatedEntry)
--                       case stts of
--                         NotActive ->
--                           seeOther ("/entries?ostatniousuniety=" ++ (show $ unEntryId pid) )
--                                                               (toResponse ())
--                         Active     -> if msg == "ok"
--												then seeOther ("/entries/view?id=" ++ (show $ unEntryId pid)++"&msg="++msg2 )
--                                                              (toResponse ())
--												else seeOther ("/entries/edit?id=" ++ (show $ unEntryId pid)++"&msg="++msg2 )
--                                                              (toResponse ())
--						]