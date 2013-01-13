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



edit :: AcidState Planner -> ServerPart Response
edit acid =
    do pid   <- SubjectId <$> lookRead "id"
       mMsg  <- optional $ lookText "msg"
       mSubject <- query' acid (SubjectById pid)
       case mSubject of
         Nothing ->
             notFound $ template "no such subject" [] $ do "Could not find a subject with id "
                                                           H.toHtml (unSubjectId pid)
         (Just p@(Subject{..})) ->
             msum [ do method GET
                       ok $ template "foo" [] $ do
                         case mMsg of
                           (Just msg) | msg == "saved" -> "Changes saved!"
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
                                   ! A.value (H.toValue title)
                           H.br
                           H.button ! A.name "status" ! A.value "Zapisz" $ "zapisz"
                  , do method POST
                       name   <- lookText' "name"
                       let updatedSubject =
                               p { title  = name
                                 }
                       update' acid (UpdateSubject updatedSubject)
                       case status of
                         Published ->
                           seeOther ("/subjects/view?id=" ++ (show $ unSubjectId pid))
                                    (toResponse ())
                  ]

                 where lookText' = fmap toStrict . lookText
-- | create a new planner subject in the database , and then redirect to /edit
new :: AcidState Planner -> ServerPart Response
new acid =
    do method POST
       now <- liftIO $ getCurrentTime
       subject <- update' acid (NewSubject now)
       seeOther ("/subjects/edit?id=" ++ show (unSubjectId $ subjectId subject)) (toResponse ())
-- | render a single planner subject into an HTML fragment
subjectHtml  :: Subject -> Html
subjectHtml (Subject{..}) =
  H.div ! A.class_ "subject" $ do
    H.h1 $ H.toHtml title
    H.div ! A.class_ "author" $ do "author: "    >> H.toHtml author
    H.div ! A.class_ "date"   $ do "published: " >> H.toHtml (show date)
    H.div ! A.class_ "tags"   $ do "tags: "       >> H.toHtml (Text.intercalate ", " tags)
    H.div ! A.class_ "bdy" $ H.toHtml body
    H.div ! A.class_ "subject-footer" $ do
     H.span $ H.a ! A.href (H.toValue $ "/subjects/view?id=" ++
                            show (unSubjectId subjectId)) $ "permalink"
     H.span $ " "
     H.span $ H.a ! A.href (H.toValue $ "/subjects/edit?id=" ++
                            show (unSubjectId subjectId)) $ "edit this subject"
-- | view a single planner subject
view :: AcidState Planner -> ServerPart Response
view acid =
    do pid <- SubjectId <$> lookRead "id"
       mSubject <- query' acid (SubjectById pid)
       case mSubject of
         Nothing ->
             notFound $ template "no such subject" [] $ do "Could not find a subject with id "
                                                           H.toHtml (unSubjectId pid)
         (Just p) ->
             ok $ template (title p) [] $ do
                 (subjectHtml p)

-- | render all the Published subjects (ordered newest to oldest)
showSubjects :: AcidState Planner -> ServerPart Response
showSubjects acid =
    do published <- query' acid (SubjectsByStatus Published)
       ok $ template "Lista przedmiotów" [] $ do
         mapM_ subjectHtml published

-- | render all the Published subjects (ordered newest to oldest)
mainPage :: AcidState Planner -> ServerPart Response
mainPage acid =
    do  ok $ template "Strona główna" [] $ do "Strona główna - wybierz coś na górze"


route :: AcidState Planner -> ServerPart Response
route acid =
    do decodeBody (defaultBodyPolicy "/tmp/" 0 1000000 1000000)
       msum [ dirs "favicon.ico"    $ notFound (toResponse ())
            , dirs "subjects/edit"  $ edit acid
            , dirs "subjects/new"    $ new acid
            , dirs "subjects/view"  $ view acid
            , dirs "subjects"        $ showSubjects acid
            , nullDir               >> mainPage acid
            ]


-- | start acid-state and the http server
main :: IO ()
main =
    do bracket (openLocalState initialPlannerState)
               (createCheckpointAndClose)
               (\acid ->
                    simpleHTTP nullConf (route acid))