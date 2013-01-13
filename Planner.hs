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
                            , defaultBodyPolicy, dir, lookRead, lookText, method
                            , notFound, nullConf, nullDir, ok, seeOther, simpleHTTP
                            , toResponse)
import Text.Blaze.Html5 (Html, (!), a, form, input, p, toHtml, label)
import Text.Blaze.Html5.Attributes (action, enctype, href, name, size, type_, value)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
newtype SubjectId = SubjectId { unSubjectId :: Integer }
    deriving (Eq, Ord, Data, Enum, Typeable, SafeCopy)
data Status =
    Draft
  | Published
    deriving (Eq, Ord, Data, Typeable)

$(deriveSafeCopy 0 'base ''Status)
data Subject = Subject
    { subjectId  :: SubjectId
    , title   :: Text
    , author  :: Text
    , body    :: Text
    , date    :: UTCTime
    , status  :: Status
    , tags    :: [Text]
    }
    deriving (Eq, Ord, Data, Typeable)

$(deriveSafeCopy 0 'base ''Subject)
newtype Title     = Title Text    deriving (Eq, Ord, Data, Typeable, SafeCopy)
newtype Author    = Author Text   deriving (Eq, Ord, Data, Typeable, SafeCopy)
newtype Tag       = Tag Text      deriving (Eq, Ord, Data, Typeable, SafeCopy)
newtype WordCount = WordCount Int deriving (Eq, Ord, Data, Typeable, SafeCopy)
instance Indexable Subject where
    empty = ixSet [ ixFun $ \bp -> [ subjectId bp ]
                  , ixFun $ \bp -> [ Title  $ title bp  ]
                  , ixFun $ \bp -> [ Author $ author bp ]
                  , ixFun $ \bp -> [ status bp ]
                  , ixFun $ \bp -> map Tag (tags bp)
                  , ixFun $ (:[]) . date  -- point-free, just for variety
                  , ixFun $ \bp -> [ WordCount (length $ Text.words $ body bp) ]
                  ]

data Blog = Blog
    { nextSubjectId :: SubjectId
    , subjects      :: IxSet Subject
    }
    deriving (Data, Typeable)

$(deriveSafeCopy 0 'base ''Blog)

initialBlogState :: Blog
initialBlogState =
    Blog { nextSubjectId = SubjectId 1
         , subjects      = empty
         }
-- | create a new, empty subject and add it to the database
newSubject :: UTCTime -> Update Blog Subject
newSubject pubDate =
    do b@Blog{..} <- get
       let subject = Subject { subjectId = nextSubjectId
                       , title  = Text.empty
                       , author = Text.empty
                       , body   = Text.empty
                       , date   = pubDate
                       , status = Draft
                       , tags   = []
                       }
       put $ b { nextSubjectId = succ nextSubjectId
               , subjects      = IxSet.insert subject subjects
               }
       return subject
-- | update the subject in the database (indexed by SubjectId)
updateSubject :: Subject -> Update Blog ()
updateSubject updatedSubject =
    do b@Blog{..} <- get
       put $ b { subjects = IxSet.updateIx (subjectId updatedSubject) updatedSubject subjects
               }
subjectById :: SubjectId -> Query Blog (Maybe Subject)
subjectById pid =
     do Blog{..} <- ask
        return $ getOne $ subjects @= pid
subjectsByStatus :: Status -> Query Blog [Subject]
subjectsByStatus status =
    do Blog{..} <- ask
       return $ IxSet.toDescList (Proxy :: Proxy UTCTime) $ subjects @= status
$(makeAcidic ''Blog
  [ 'newSubject
  , 'updateSubject
  , 'subjectById
  , 'subjectsByStatus
  ])
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
         H.li $ H.a ! A.href "/" $ "home"
         H.li $ H.a ! A.href "/drafts" $ "drafts"
         H.li $ H.form ! A.enctype "multipart/form-data"
                       ! A.method "POST"
                       ! A.action "/new" $ H.button $ "new subject"
        body

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

edit :: AcidState Blog -> ServerPart Response
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
                              ! A.action (H.toValue $ "/edit?id=" ++
                                                      (show $ unSubjectId pid)) $ do
                           H.label "title" ! A.for "title"
                           H.input ! A.type_ "text"
                                   ! A.name "title"
                                   ! A.id "title"
                                   ! A.size "80"
                                   ! A.value (H.toValue title)
                           H.br
                           H.label "author" ! A.for "author"
                           H.input ! A.type_ "text"
                                   ! A.name "author"
                                   ! A.id "author"
                                   ! A.size "40"
                                   ! A.value (H.toValue author)
                           H.br
                           H.label "tags" ! A.for "tags"
                           H.input ! A.type_ "text"
                                   ! A.name "tags"
                                   ! A.id "tags"
                                   ! A.size "40"
                                   ! A.value (H.toValue $ Text.intercalate ", " tags)
                           H.br
                           H.label "body" ! A.for "body"
                           H.br
                           H.textarea ! A.cols "80" ! A.rows "20" ! A.name "body" $ H.toHtml body
                           H.br
                           H.button ! A.name "status" ! A.value "publish" $ "publish"
                           H.button ! A.name "status" ! A.value "save"    $ "save as draft"
                  , do method POST
                       ttl   <- lookText' "title"
                       athr  <- lookText' "author"
                       tgs   <- lookText' "tags"

                       bdy   <- lookText' "body"
                       now   <- liftIO $ getCurrentTime
                       stts  <- do s <- lookText' "status"
                                   case s of
                                      "save"    -> return Draft
                                      "publish" -> return Published
                                      _         -> mzero
                       let updatedSubject =
                               p { title  = ttl
                                 , author = athr
                                 , body   = bdy
                                 , date   = now
                                 , status = stts
                                 , tags   = map Text.strip $ Text.splitOn "," tgs
                                 }
                       update' acid (UpdateSubject updatedSubject)
                       case status of
                         Published ->
                           seeOther ("/view?id=" ++ (show $ unSubjectId pid))
                                    (toResponse ())
                         Draft     ->
                           seeOther ("/edit?msg=saved&id=" ++ (show $ unSubjectId pid))
                                    (toResponse ())
                  ]

                 where lookText' = fmap toStrict . lookText
-- | create a new blog subject in the database , and then redirect to /edit
new :: AcidState Blog -> ServerPart Response
new acid =
    do method POST
       now <- liftIO $ getCurrentTime
       subject <- update' acid (NewSubject now)
       seeOther ("/edit?id=" ++ show (unSubjectId $ subjectId subject)) (toResponse ())
-- | render a single blog subject into an HTML fragment
subjectHtml  :: Subject -> Html
subjectHtml (Subject{..}) =
  H.div ! A.class_ "subject" $ do
    H.h1 $ H.toHtml title
    H.div ! A.class_ "author" $ do "author: "    >> H.toHtml author
    H.div ! A.class_ "date"   $ do "published: " >> H.toHtml (show date)
    H.div ! A.class_ "tags"   $ do "tags: "       >> H.toHtml (Text.intercalate ", " tags)
    H.div ! A.class_ "bdy" $ H.toHtml body
    H.div ! A.class_ "subject-footer" $ do
     H.span $ H.a ! A.href (H.toValue $ "/view?id=" ++
                            show (unSubjectId subjectId)) $ "permalink"
     H.span $ " "
     H.span $ H.a ! A.href (H.toValue $ "/edit?id=" ++
                            show (unSubjectId subjectId)) $ "edit this subject"
-- | view a single blog subject
view :: AcidState Blog -> ServerPart Response
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
home :: AcidState Blog -> ServerPart Response
home acid =
    do published <- query' acid (SubjectsByStatus Published)
       ok $ template "home" [] $ do
         mapM_ subjectHtml published
-- | show a list of all unpublished blog subjects
drafts :: AcidState Blog -> ServerPart Response
drafts acid =
    do drafts <- query' acid (SubjectsByStatus Draft)
       case drafts of
         [] -> ok $ template "drafts" [] $ "You have no unpublished subjects at this time."
         _ ->
             ok $ template "home" [] $
                 H.ol $ mapM_ editDraftLink drafts
    where
      editDraftLink Subject{..} =
        H.a ! A.href (H.toValue $ "/edit?id=" ++ show (unSubjectId subjectId)) $ H.toHtml title
-- | route incoming requests



route :: AcidState Blog -> ServerPart Response
route acid =
    do decodeBody (defaultBodyPolicy "/tmp/" 0 1000000 1000000)
       msum [ dir "favicon.ico" $ notFound (toResponse ())
            , dir "edit"        $ edit acid
            , dir "new"         $ new acid
            , dir "view"        $ view acid
            , dir "drafts"      $ drafts acid
            , nullDir          >> home acid
            ]
-- | start acid-state and the http server
main :: IO ()
main =
    do bracket (openLocalState initialBlogState)
               (createCheckpointAndClose)
               (\acid ->
                    simpleHTTP nullConf (route acid))