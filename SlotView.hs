{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, RecordWildCards
  , TemplateHaskell, TypeFamilies, OverloadedStrings #-}
module SlotView where
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

editSlotView :: AcidState Planner -> ServerPart Response
editSlotView acid =
    do pid   <- SlotId <$> lookRead "id"
       mMsg  <- optional $ lookText "msg"
       mSlot <- query' acid (SlotById pid)
       case mSlot of
         Nothing ->
             notFound $ template "no such slot" [] $ do "Nie Można odnaleźć slotu o ID: "
                                                        H.toHtml (unSlotId pid)
         (Just p@(Slot{..})) ->
             msum [ do method GET
                       ok $ template "foo" [] $ do
                         case mMsg of
                           (Just msg) | msg == "saved" -> "Zmiany zapisane!"
                           _ -> ""
                         H.form ! A.enctype "multipart/form-data"
                              ! A.method "POST"
                              ! A.action (H.toValue $ "/slots/edit?id=" ++
                                                      (show $ unSlotId pid)) $ do
                           H.label "Godzina rozpoczęcia" ! A.for "startTime"
                           H.select ! A.name "startTime" $ do
                              H.option  "Poniedziałek - 8.00"
                                     ! A.value "Poniedziałek - 8.00"
                              H.option  "Poniedziałek - 9.00"
                                     ! A.value "Poniedziałek - 9.00"
                              H.option  "Poniedziałek - 10.00"
                                     ! A.value "Poniedziałek - 10.00"
                              H.option  "Poniedziałek - 11.00"
                                     ! A.value "Poniedziałek - 11.00"
                              H.option  "Poniedziałek - 12.00"
                                     ! A.value "Poniedziałek - 12.00"
                              H.option  "Poniedziałek - 13.00"
                                     ! A.value "Poniedziałek - 13.00"
                              H.option  "Poniedziałek - 14.00"
                                     ! A.value "Poniedziałek - 14.00"
                              H.option  "Poniedziałek - 15.00"
                                     ! A.value "Poniedziałek - 15.00"
                              H.option  "Poniedziałek - 16.00"
                                     ! A.value "Poniedziałek - 16.00"
                              H.option  "Wtorek - 8.00"
                                     ! A.value "Wtorek - 8.00"
                              H.option  "Wtorek - 9.00"
                                     ! A.value "Wtorek - 9.00"
                              H.option  "Wtorek - 10.00"
                                     ! A.value "Wtorek - 10.00"
                              H.option  "Wtorek - 11.00"
                                     ! A.value "Wtorek - 11.00"
                              H.option  "Wtorek - 12.00"
                                     ! A.value "Wtorek - 12.00"
                              H.option  "Wtorek - 13.00"
                                     ! A.value "Wtorek - 13.00"
                              H.option  "Wtorek - 14.00"
                                     ! A.value "Wtorek - 14.00"
                              H.option  "Wtorek - 15.00"
                                     ! A.value "Wtorek - 15.00"
                              H.option  "Wtorek - 16.00"
                                     ! A.value "Wtorek - 16.00"
                              H.option  "Środa - 8.00"
                                     ! A.value "Środa - 8.00"
                              H.option  "Środa - 9.00"
                                     ! A.value "Środa - 9.00"
                              H.option  "Środa - 10.00"
                                     ! A.value "Środa - 10.00"
                              H.option  "Środa - 11.00"
                                     ! A.value "Środa - 11.00"
                              H.option  "Środa - 12.00"
                                     ! A.value "Środa - 12.00"
                              H.option  "Środa - 13.00"
                                     ! A.value "Środa - 13.00"
                              H.option  "Środa - 14.00"
                                     ! A.value "Środa - 14.00"
                              H.option  "Środa - 15.00"
                                     ! A.value "Środa - 15.00"
                              H.option  "Środa - 16.00"
                                     ! A.value "Środa - 16.00"
                              H.option  "Czwartek - 8.00"
                                     ! A.value "Czwartek - 8.00"
                              H.option  "Czwartek - 9.00"
                                     ! A.value "Czwartek - 9.00"
                              H.option  "Czwartek - 10.00"
                                     ! A.value "Czwartek - 10.00"
                              H.option  "Czwartek - 11.00"
                                     ! A.value "Czwartek - 11.00"
                              H.option  "Czwartek - 12.00"
                                     ! A.value "Czwartek - 12.00"
                              H.option  "Czwartek - 13.00"
                                     ! A.value "Czwartek - 13.00"
                              H.option  "Czwartek - 14.00"
                                     ! A.value "Czwartek - 14.00"
                              H.option  "Czwartek - 15.00"
                                     ! A.value "Czwartek - 15.00"
                              H.option  "Czwartek - 16.00"
                                     ! A.value "Czwartek - 16.00"
                              H.option  "Piątek - 8.00"
                                     ! A.value "Piątek - 8.00"
                              H.option  "Piątek - 9.00"
                                     ! A.value "Piątek - 9.00"
                              H.option  "Piątek - 10.00"
                                     ! A.value "Piątek - 10.00"
                              H.option  "Piątek - 11.00"
                                     ! A.value "Piątek - 11.00"
                              H.option  "Piątek - 12.00"
                                     ! A.value "Piątek - 12.00"
                              H.option  "Piątek - 13.00"
                                     ! A.value "Piątek - 13.00"
                              H.option  "Piątek - 14.00"
                                     ! A.value "Piątek - 14.00"
                              H.option  "Piątek - 15.00"
                                     ! A.value "Piątek - 15.00"
                              H.option  "Piątek - 16.00"
                                     ! A.value "Piątek - 16.00"
                              H.br
                           H.button ! A.name "status" ! A.value "Zapisz" $ "zapisz"
                  , do method POST
                       startTime   <- lookText' "startTime"
                       let updatedSlot =
                               p { slotTime  = startTime
                                 }
                       update' acid (UpdateSlot updatedSlot)
                       seeOther ("/slots/view?id=" ++ (show $ unSlotId pid))
                                    (toResponse ())
                  ]

                 where lookText' = fmap toStrict . lookText
-- | create a new planner slot in the database , and then redirect to /edit
newSlotView :: AcidState Planner -> ServerPart Response
newSlotView acid =
    do method POST
       now <- liftIO $ getCurrentTime
       slot <- update' acid (NewSlot now)
       seeOther ("/slots/edit?id=" ++ show (unSlotId $ slotId slot)) (toResponse ())
-- | render a single planner slot into an HTML fragment
slotHtml  :: Slot -> Html
slotHtml (Slot{..}) =
  H.div ! A.class_ "slot" $ do
    H.h1 $ H.toHtml slotTime
    H.div ! A.class_ "post-footer" $ do
     H.span $ H.a ! A.href (H.toValue $ "/slots/view?id=" ++
                            show (unSlotId slotId)) $ "Otwórz"
     H.span $ " "
     H.span $ H.a ! A.href (H.toValue $ "/slots/edit?id=" ++
                            show (unSlotId slotId)) $ "Edytuj ten slot"
-- | view a single planner slot
viewSlot :: AcidState Planner -> ServerPart Response
viewSlot acid =
    do pid <- SlotId <$> lookRead "id"
       mSlot <- query' acid (SlotById pid)
       case mSlot of
         Nothing ->
             notFound $ template "no such slot" [] $ do "Could not find a slot with id "
                                                        H.toHtml (unSlotId pid)
         (Just p) ->
             ok $ template (slotTime p) [] $ do
                 (slotHtml p)

showSlots :: AcidState Planner -> ServerPart Response
showSlots acid =
    do allSlots <- query' acid (SlotsAll)
       ok $ template "Lista sal" [] $ do
         mapM_ slotHtml allSlots

slotInfo :: AcidState Planner -> ServerPart Response
slotInfo acid =
    do pid <- SlotId <$> lookRead "id"
       mSlot <- query' acid (SlotById pid)
       case mSlot of
         Nothing ->
             ok $ toResponse $ H.div "Nie znaleziona slotu"
         (Just p) ->
             ok $ toResponse $ (slotHtml2 p)


slotHtml2  :: Slot -> Html
slotHtml2 (Slot{..}) =
    (H.toHtml slotTime)


