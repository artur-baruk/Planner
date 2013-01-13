{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, RecordWildCards
  , TemplateHaskell, TypeFamilies, OverloadedStrings #-}
module RoomView where
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

editRoomView :: AcidState Planner -> ServerPart Response
editRoomView acid =
    do pid   <- RoomId <$> lookRead "id"
       mMsg  <- optional $ lookText "msg"
       mRoom <- query' acid (RoomById pid)
       case mRoom of
         Nothing ->
             notFound $ template "no such room" [] $ do "Nie Można odnaleźć sali o ID: "
                                                        H.toHtml (unRoomId pid)
         (Just p@(Room{..})) ->
             msum [ do method GET
                       ok $ template "foo" [] $ do
                         case mMsg of
                           (Just msg) | msg == "saved" -> "Zmiany zapisane!"
                           _ -> ""
                         H.form ! A.enctype "multipart/form-data"
                              ! A.method "POST"
                              ! A.action (H.toValue $ "/rooms/edit?id=" ++
                                                      (show $ unRoomId pid)) $ do
                           H.label "Nazwa" ! A.for "Nazwa"
                           H.input ! A.type_ "text"
                                   ! A.name "name"
                                   ! A.id "name"
                                   ! A.size "80"
                                   ! A.value (H.toValue roomName)
                           H.br
                           H.button ! A.name "status" ! A.value "Zapisz" $ "zapisz"
                  , do method POST
                       name   <- lookText' "name"
                       let updatedRoom =
                               p { roomName  = name
                                 }
                       update' acid (UpdateRoom updatedRoom)
                       seeOther ("/rooms/view?id=" ++ (show $ unRoomId pid))
                                    (toResponse ())
                  ]

                 where lookText' = fmap toStrict . lookText
-- | create a new planner room in the database , and then redirect to /edit
newRoomView :: AcidState Planner -> ServerPart Response
newRoomView acid =
    do method POST
       now <- liftIO $ getCurrentTime
       room <- update' acid (NewRoom now)
       seeOther ("/rooms/edit?id=" ++ show (unRoomId $ roomId room)) (toResponse ())
-- | render a single planner room into an HTML fragment
roomHtml  :: Room -> Html
roomHtml (Room{..}) =
  H.div ! A.class_ "room" $ do
    H.h1 $ H.toHtml roomName
    H.div ! A.class_ "post-footer" $ do
     H.span $ H.a ! A.href (H.toValue $ "/rooms/view?id=" ++
                            show (unRoomId roomId)) $ "Otwórz"
     H.span $ " "
     H.span $ H.a ! A.href (H.toValue $ "/rooms/edit?id=" ++
                            show (unRoomId roomId)) $ "Edytuj tą salę"
-- | view a single planner room
viewRoom :: AcidState Planner -> ServerPart Response
viewRoom acid =
    do pid <- RoomId <$> lookRead "id"
       mRoom <- query' acid (RoomById pid)
       case mRoom of
         Nothing ->
             notFound $ template "no such room" [] $ do "Could not find a room with id "
                                                        H.toHtml (unRoomId pid)
         (Just p) ->
             ok $ template (roomName p) [] $ do
                 (roomHtml p)

showRooms :: AcidState Planner -> ServerPart Response
showRooms acid =
    do allRooms <- query' acid (RoomsAll)
       ok $ template "Lista sal" [] $ do
         mapM_ roomHtml allRooms
