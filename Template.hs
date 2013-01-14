{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, RecordWildCards
  , TemplateHaskell, TypeFamilies, OverloadedStrings #-}
module Template where
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
import Text.Blaze.Html5 (Html, (!), a, form, input, p, toHtml, label)
import Text.Blaze.Html5.Attributes (action, enctype, href, name, size, type_, value)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
-- | CSS for our site
--
-- Normally this would live in an external .css file.
-- It is included inline here to keep the example self-contained.
css :: Html
css =
    let s = Text.concat [ "body { color: #555; padding: 0; margin: 0; margin-left: 1em;}"
                        , "ul { list-style-type: none; }"
                        , "ol { list-style-type: none; }"
                        , "h1 { font-size: 1.5em; color: #555; margin: 0; }"
                        , ".author { color: #aaa; }"
                        , ".date { color: #aaa; }"
                        , ".tags { color: #aaa; }"
                        , ".subject { border-bottom: 1px dotted #aaa; margin-top: 1em; }"
                        , ".bdy  { color: #555; margin-top: 1em; }"
                        , ".subject-footer { margin-top: 1em; margin-bottom: 1em; }"
                        , "label { display: inline-block; width: 3em; }"
                        , "#menu { margin: 0; padding: 0; margin-left: -1em;"
                        ,         "border-bottom: 1px solid #aaa; }"
                        , "#menu li { display: inline; margin-left: 1em; }"
                        , "#menu form { display: inline; margin-left: 1em; }"
                        ]
    in H.style ! A.type_ "text/css" $ H.toHtml s

-- | HTML template that we use to render all the pages on the site
template :: Text -> [Html] -> Html -> Response
template title headers body =
  toResponse $
    H.html $ do
      H.head $ do
        css
        H.title (H.toHtml title)
        H.meta ! A.httpEquiv "Content-Type" ! A.content "text/html;charset=utf-8"
        sequence_ headers
      H.body $ do
        H.ul ! A.id "menu" $ do
         H.li $ H.a ! A.href "/subjects" $ "Przedmioty"
         H.li $ H.a ! A.href "/groups" $ "Grupy"
         H.li $ H.a ! A.href "/rooms" $ "Sale"
         H.li $ H.a ! A.href "/slots" $ "Sloty godzinowe"
         H.li $ H.a ! A.href "/plan" $ "Plan - wpisy"
         H.li $ H.a ! A.href "/planTable" $ "Plan - tabelka"
         H.li $ H.a ! A.href "/settings" $ "Ustawienia"

         H.li $ H.form ! A.enctype "multipart/form-data"
                       ! A.method "POST"
                       ! A.action "/subjects/new" $ H.button $ "Nowy przedmiot"
         H.li $ H.form ! A.enctype "multipart/form-data"
                       ! A.method "POST"
                       ! A.action "/groups/new" $ H.button $ "Nowa grupa"
         H.li $ H.form ! A.enctype "multipart/form-data"
                       ! A.method "POST"
                       ! A.action "/rooms/new" $ H.button $ "Nowa sala"
         H.li $ H.form ! A.enctype "multipart/form-data"
                       ! A.method "POST"
                       ! A.action "/slots/new" $ H.button $ "Nowy slot"
         H.li $ H.form ! A.enctype "multipart/form-data"
                       ! A.method "POST"
                       ! A.action "/plan/add" $ H.button $ "Dodaj wpis do planu"
         H.li $ H.form ! A.enctype "multipart/form-data"
                       ! A.method "POST"
                       ! A.action "/plan/automatic" $ H.button $ "Automatyczne układanie planu"
        body
