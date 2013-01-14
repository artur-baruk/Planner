{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, RecordWildCards
  , TemplateHaskell, TypeFamilies, OverloadedStrings #-}
module SettingsView where
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


--editSettingsView :: AcidState Planner -> ServerPart Response
--editSettingsView acid =
--    do mMsg  <- optional $ lookText "msg"
--       mSettings <- query' acid PeekSettings
--       case mSettings of
--         Nothing ->
--             notFound $ template "no such subject" [] $ do "Nie Mozna odnalezc ustawien: "
--                                                           H.toHtml (1)
--         (Just p@(Settings{..})) ->
--             msum [ do method GET
--                       ok $ template "foo" [] $ do
--                         case mMsg of
--                           (Just msg) | msg == "saved" -> "Zmiany zapisane!"
--                           _ -> ""
--                         H.form ! A.enctype "multipart/form-data"
--                             ! A.method "POST"
--                             ! A.action (H.toValue $ "/settings" $ do
--                          H.label "Godzin per grupa" ! A.for "hPw"
--                          H.input ! A.type_ "number"
--                               ! A.name "maxDailyHoursPerGroup"
--                               ! A.id "maxDailyHoursPerGroup"
--                               ! A.value (H.toValue maxDailyHoursPerGroup)
--                          H.br
--                          H.label "Godzina start" ! A.for "hPw2"
--                          H.input ! A.type_ "number"
--                               ! A.name "startTime"
--                               ! A.id "startTime"
--                               ! A.value (H.toValue startTime)
--                          H.br
--                          H.label "Godzina end" ! A.for "hPw3"
--                          H.input ! A.type_ "number"
--                               ! A.name "endTime"
--                               ! A.id "endTime"
--                               ! A.value (H.toValue endTime)
--                          H.br
--                          H.button ! A.name "status" ! A.value "Zapisz" $ "zapisz"
--                  , do method POST
--                       mMaxDailyHoursPerGroup <- lookText' "maxDailyHoursPerGroup"
--                       mStartTime <- lookText' "startTime"
--                       mEndTime <- lookRead "endTime"
--                       let updatedSettings =
--                               p { subjectName  = mMaxDailyHoursPerGroup
--                                 ,  subjectDesc = mStartTime
--                                 ,  hoursPerWeek =  mEndTime
--                                 }
--                       update' acid (UpdateSettings updatedSettings)
--                       seeOther "/subjects" (toResponse ())
--                  ]
--                 where lookText' = fmap toStrict . lookText
    
-- | render settings into an HTML fragment
settingsHtml  :: Settings -> Html
settingsHtml (Settings{..}) =
  H.div ! A.class_ "settings" $ do
    H.div ! A.class_ "maxDailyHoursPerGroup" $ do "Max dzienna liczba zajec dla grupy: "    >> H.toHtml maxDailyHoursPerGroup
    H.div ! A.class_ "startTime" $ do "Godzina rozpoczecia zajec: "    >> H.toHtml startTime
    H.div ! A.class_ "godzin" $ do "Godzina zakonczenia zajec: "    >> H.toHtml endTime

-- | view a single planner subject
viewSettings :: AcidState Planner -> ServerPart Response
viewSettings acid =
    do mSett <- query' acid PeekSettings
       ok $ template "settings" [] $ do
         settingsHtml mSett