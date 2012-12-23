{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}


module Subject where
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
 
 subject :: ServerPart Response
 subject = msum
           [   dir "add" $ addSubjectGet
             , dir "add" $ addSubjectPost
             , subjectList 
           ]  
             
 
 subjectList :: ServerPart Response
 subjectList =
  ok $ template "Lista przedmiotów" $ do
    H.h2 "Lista przedmiotów"
    
 
 addSubjectGet :: ServerPart Response
 addSubjectGet =
         do method GET
            ok $ template "form" $
               form ! action "/subject/add" ! enctype "multipart/form-data" ! A.method "POST" $ do
                 H.h2 "Wprowadź dane nowego przedmiotu"
                 label ! A.for "subject_id_label" $ "ID przedmiotu: " 
                 input ! type_ "text" ! A.id "subject_id" ! name "subject_id"
                 H.br
                 label ! A.for "subject_name_label" $ "Nazwa przedmiotu: " 
                 input ! type_ "text" ! A.id "subject_name" ! name "subject_name"               
                 H.br
                 input ! type_ "submit" ! value "Zapisz"

 addSubjectPost :: ServerPart Response
 addSubjectPost =
         do method POST
            subject_id <- lookText "subject_id"
            subject_name <- lookText "subject_name"
            ok $ template "form" $ do
              H.h2 "Przedmiot dodany"
              H.p "Id przedmiotu:"
              H.p (toHtml subject_id)
              H.p "Nazwa przedmiotu:"
              H.p (toHtml subject_name)
