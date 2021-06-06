module Monadoc.Handler.Common where

import Monadoc.Prelude

import qualified Data.CaseInsensitive as CI
import qualified Data.Maybe as Maybe
import qualified Data.Pool as Pool
import qualified Data.UUID as Uuid
import qualified Monadoc.Class.ToXml as ToXml
import qualified Monadoc.Model.Session as Session
import qualified Monadoc.Model.User as User
import qualified Monadoc.Server.Response as Response
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Route as Route
import qualified Monadoc.Type.Version as Version
import qualified Monadoc.Utility.Xml as Xml
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Text.XML as Xml
import qualified Web.Cookie as Cookie

getUser :: Context.Context -> Wai.Request -> IO (Maybe User.User)
getUser context request = do
    let
        cookies = Cookie.parseCookies . Maybe.fromMaybe mempty . lookup Http.hCookie $ Wai.requestHeaders request
    case lookup (into @ByteString "guid") cookies of
        Just byteString -> case fmap (from @Uuid.UUID) $ Uuid.fromASCIIBytes byteString of
            Just guid -> Pool.withResource (Context.pool context) $ \ connection -> do
                    maybeSession <- Session.selectByGuid connection guid
                    case maybeSession of
                        Just session -> User.selectByGithubId connection $ Session.userGithubId session
                        Nothing -> pure Nothing
            _ -> pure Nothing
        _ -> pure Nothing

makeResponse :: ToXml.ToXml page => Monadoc page -> Wai.Response
makeResponse monadoc =
    let
        baseUrl = config_baseUrl $ monadoc_config monadoc
        status = Http.ok200
        hLink = CI.mk $ into @ByteString "Link"
        link = into @ByteString $ "<" <> baseUrl <> Route.toString Route.Bootstrap <> ">; rel=preload; as=style"
        headers = [(hLink, link)]
        instruction = Xml.Instruction
            (into @Text "xml-stylesheet")
            (into @Text $ "type=\"text/xsl\" charset=\"UTF-8\" href=\"" <> Xml.escape (baseUrl <> Route.toString Route.Template) <> "\"")
        prologue = Xml.Prologue [Xml.MiscInstruction instruction] Nothing []
        root = Xml.element "monadoc" []
            [ ToXml.toXml $ monadoc_config monadoc
            , Xml.node "page" [] [ToXml.toXml $ monadoc_page monadoc]
            ]
        epilogue = [] :: [Xml.Miscellaneous]
        document = Xml.Document prologue root epilogue
    in Response.xml status headers document

data Monadoc page = Monadoc
    { monadoc_config :: Config
    , monadoc_page :: page
    } deriving (Eq, Show)

data Config = Config
    { config_baseUrl :: String
    , config_breadcrumbs :: [Breadcrumb]
    , config_clientId :: String
    , config_user :: Maybe String
    , config_version :: Version.Version
    } deriving (Eq, Show)

instance ToXml.ToXml Config where
    toXml config = Xml.node "config" []
        [ Xml.node "baseUrl" [] [ToXml.toXml $ config_baseUrl config]
        , Xml.node "breadcrumbs" [] . fmap ToXml.toXml $ config_breadcrumbs config
        , Xml.node "clientId" [] [ToXml.toXml $ config_clientId config]
        , Xml.node "user" [] [ToXml.toXml $ config_user config]
        , Xml.node "version" [] [ToXml.toXml $ config_version config]
        ]

data Breadcrumb = Breadcrumb
    { breadcrumb_name :: String
    , breadcrumb_route :: Maybe Route.Route
    } deriving (Eq, Show)

instance ToXml.ToXml Breadcrumb where
    toXml breadcrumb = Xml.node "breadcrumb" []
        [ Xml.node "name" [] [ToXml.toXml $ breadcrumb_name breadcrumb]
        , Xml.node "route" [] [ToXml.toXml $ breadcrumb_route breadcrumb]
        ]
