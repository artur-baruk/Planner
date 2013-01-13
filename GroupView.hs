{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, RecordWildCards
  , TemplateHaskell, TypeFamilies, OverloadedStrings #-}
module GroupView where
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

editGroupView :: AcidState Planner -> ServerPart Response
editGroupView acid =
    do pid   <- GroupId <$> lookRead "id"
       mMsg  <- optional $ lookText "msg"
       mGroup <- query' acid (GroupById pid)
       case mGroup of
         Nothing ->
             notFound $ template "no such group" [] $ do "Nie Można odnaleźć grupy o ID: "
                                                         H.toHtml (unGroupId pid)
         (Just p@(Group{..})) ->
             msum [ do method GET
                       ok $ template "foo" [] $ do
                         case mMsg of
                           (Just msg) | msg == "saved" -> "Zmiany zapisane!"
                           _ -> ""
                         H.form ! A.enctype "multipart/form-data"
                              ! A.method "POST"
                              ! A.action (H.toValue $ "/groups/edit?id=" ++
                                                      (show $ unGroupId pid)) $ do
                           H.label "Nazwa" ! A.for "Nazwa"
                           H.input ! A.type_ "text"
                                   ! A.name "name"
                                   ! A.id "name"
                                   ! A.size "80"
                                   ! A.value (H.toValue groupName)
                           H.br
                           H.button ! A.name "status" ! A.value "Zapisz" $ "zapisz"
                  , do method POST
                       name   <- lookText' "name"
                       let updatedGroup =
                               p { groupName  = name
                                 }
                       update' acid (UpdateGroup updatedGroup)
                       seeOther ("/groups/view?id=" ++ (show $ unGroupId pid))
                                    (toResponse ())
                  ]

                 where lookText' = fmap toStrict . lookText
-- | create a new planner group in the database , and then redirect to /edit
newGroupView :: AcidState Planner -> ServerPart Response
newGroupView acid =
    do method POST
       now <- liftIO $ getCurrentTime
       group <- update' acid (NewGroup now)
       seeOther ("/groups/edit?id=" ++ show (unGroupId $ groupId group)) (toResponse ())
-- | render a single planner group into an HTML fragment
groupHtml  :: Group -> Html
groupHtml (Group{..}) =
  H.div ! A.class_ "group" $ do
    H.h1 $ H.toHtml groupName
    H.div ! A.class_ "post-footer" $ do
     H.span $ H.a ! A.href (H.toValue $ "/groups/view?id=" ++
                            show (unGroupId groupId)) $ "Otwórz"
     H.span $ " "
     H.span $ H.a ! A.href (H.toValue $ "/groups/edit?id=" ++
                            show (unGroupId groupId)) $ "Edytuj tą grupę"
-- | view a single planner group
viewGroup :: AcidState Planner -> ServerPart Response
viewGroup acid =
    do pid <- GroupId <$> lookRead "id"
       mGroup <- query' acid (GroupById pid)
       case mGroup of
         Nothing ->
             notFound $ template "no such group" [] $ do "Could not find a group with id "
                                                         H.toHtml (unGroupId pid)
         (Just p) ->
             ok $ template (groupName p) [] $ do
                 (groupHtml p)

showGroups :: AcidState Planner -> ServerPart Response
showGroups acid =
    do allGroups <- query' acid (GroupsAll)
       ok $ template "Lista grup" [] $ do
         mapM_ groupHtml allGroups
