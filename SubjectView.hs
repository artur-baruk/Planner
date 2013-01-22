{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, RecordWildCards
  , TemplateHaskell, TypeFamilies, OverloadedStrings #-}
module SubjectView where
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


editSubjectView :: AcidState Planner -> ServerPart Response
editSubjectView acid =
    do pid   <- SubjectId <$> lookRead "id"
       mMsg  <- optional $ lookText "msg"
       mSubject <- query' acid (SubjectById pid)
       case mSubject of
         Nothing ->
             notFound $ template "no such subject" [] $ do "Nie Można odnaleźć przedmiotu o ID: "
                                                           H.toHtml (unSubjectId pid)
         (Just p@(Subject{..})) ->
             msum [ do method GET
                       ok $ template "foo" [] $ do
                         case mMsg of
                           (Just msg) | msg == "saved" -> "Zmiany zapisane!"
                           _ -> ""
                         H.form ! A.enctype "multipart/form-data"
                              ! A.method "POST"
                              ! A.action (H.toValue $ "/subjects/edit?id=" ++
                                                      (show $ unSubjectId pid)) $ do
                           H.label "Nazwa" ! A.for "Nazwa"
                           H.input ! A.type_ "text"
                                   ! A.name "name"
                                   ! A.id "name"
                                   ! A.size "80"
                                   ! A.value (H.toValue subjectName)
                           H.br
                           H.label "Opis" ! A.for "opis"
                           H.input ! A.type_ "text"
                                   ! A.name "desc"
                                   ! A.id "desc"
                                   ! A.size "80"
                                   ! A.value (H.toValue subjectDesc)
                           H.br
                           H.label "Godzin" ! A.for "hPw"
                           H.input ! A.type_ "number"
                                   ! A.name "hoursPerWeek"
                                   ! A.id "hoursPerWeek"
                                   ! A.value (H.toValue hoursPerWeek)
                           H.br
                           H.button ! A.name "status" ! A.value "Zapisz" $ "zapisz"
                  , do method POST
                       name   <- lookText' "name"
                       desc   <- lookText' "desc"
                       hoursPerWeek <-   lookRead "hoursPerWeek"
                       let updatedSubject =
                               p { subjectName  = name
                                 ,  subjectDesc = desc
                                 ,  hoursPerWeek =  hoursPerWeek
                                 }
                       update' acid (UpdateSubject updatedSubject)
                       seeOther ("/subjects/view?id=" ++ (show $ unSubjectId pid))
                                    (toResponse ())
                  ]

                 where lookText' = fmap toStrict . lookText
-- | create a new planner subject in the database , and then redirect to /edit
newSubjectView :: AcidState Planner -> ServerPart Response
newSubjectView acid =
    do method POST
       now <- liftIO $ getCurrentTime
       subject <- update' acid (NewSubject now)
       seeOther ("/subjects/edit?id=" ++ show (unSubjectId $ subjectId subject)) (toResponse ())
-- | render a single planner subject into an HTML fragment
subjectHtml  :: Subject -> Html
subjectHtml (Subject{..}) =
  H.div ! A.class_ "subject" $ do
    H.h1 $ H.toHtml subjectName
    H.div ! A.class_ "opis" $ do "Opis: "    >> H.toHtml subjectDesc
    H.div ! A.class_ "godzin" $ do "Godzin w tygodniu: "    >> H.toHtml hoursPerWeek
    H.div ! A.class_ "post-footer" $ do
     H.span $ H.a ! A.href (H.toValue $ "/subjects/view?id=" ++
                            show (unSubjectId subjectId)) $ "Otwórz"
     H.span $ " "
     H.span $ H.a ! A.href (H.toValue $ "/subjects/edit?id=" ++
                            show (unSubjectId subjectId)) $ "Edytuj ten przedmiot"
-- | view a single planner subject
viewSubject :: AcidState Planner -> ServerPart Response
viewSubject acid =
    do pid <- SubjectId <$> lookRead "id"
       mSubject <- query' acid (SubjectById pid)
       case mSubject of
         Nothing ->
             notFound $ template "no such subject" [] $ do "Could not find a subject with id "
                                                           H.toHtml (unSubjectId pid)
         (Just p) ->
             ok $ template (subjectName p) [] $ do
                 (subjectHtml p)

showSubjects :: AcidState Planner -> ServerPart Response
showSubjects acid =
    do allSubjects <- query' acid (SubjectsAll)
       ok $ template "Lista przedmiotów" [] $ do
         mapM_ subjectHtml allSubjects
