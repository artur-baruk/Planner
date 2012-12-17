
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}


module PlannerTemplate where
 import Control.Applicative ((<$>), optional)
 import Data.Maybe (fromMaybe)
 import Data.Text (Text)
 import Data.Text.Lazy (unpack)
 import Happstack.Lite
 import Text.Blaze.Html5 (Html, (!), a, form, input, p, toHtml, label)
 import Text.Blaze.Html5.Attributes (action, enctype, href, name, size, type_, value)
 import qualified Text.Blaze.Html5 as H
 import qualified Text.Blaze.Html5.Attributes as A
 
 
 
 template :: Text -> Html -> Response
 template title body = toResponse $
   H.html $ do
     H.head $ do
       H.title (toHtml title)
     H.body $ do
       body
       p $ a ! href "/" $ "Strona główna"

 