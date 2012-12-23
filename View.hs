{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}


module View where
 import Control.Applicative ((<$>), optional)
 import Data.Maybe (fromMaybe)
 import Data.Text (Text)
 import Data.Text.Lazy (unpack)
 import Happstack.Lite
 import Text.Blaze.Html5 (Html, (!), a, form, input, p, toHtml, label)
 import Text.Blaze.Html5.Attributes (action, enctype, href, name, size, type_, value)
 import qualified Text.Blaze.Html5 as H
 import qualified Text.Blaze.Html5.Attributes as A
 import PlannerTemplate
 import SubjectView 
 
 
 homePage :: ServerPart Response
 homePage =
  ok $ template "Plan zajęć" $ do
  H.h2 "Aplikacja do układania tygodniowego planu zajęć"
  H.p $ a ! href "/subject" $ "Lista przedmiotów"  
  H.p $ a ! href "/subject/add" $ "Dodaj przedmiot" 