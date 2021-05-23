module Monadoc.Server.Application where

import qualified Control.Monad.Catch as Exception
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Maybe as Maybe
import qualified Data.Pool as Pool
import qualified Data.Time as Time
import qualified Data.UUID as Uuid
import qualified Monadoc.Exception.InvalidJson as InvalidJson
import qualified Monadoc.Exception.MissingCode as MissingCode
import qualified Monadoc.Model.Session as Session
import qualified Monadoc.Model.User as User
import qualified Monadoc.Server.Response as Response
import qualified Monadoc.Server.Settings as Settings
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.GithubUser as GithubUser
import qualified Monadoc.Type.Guid as Guid
import qualified Monadoc.Type.OAuthResponse as OAuthResponse
import qualified Monadoc.Utility.Convert as Convert
import qualified Monadoc.Utility.Xml as Xml
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Types as Http
import qualified Network.HTTP.Types.Header as Http
import qualified Network.Wai as Wai
import qualified Paths_monadoc as Package
import qualified System.FilePath as FilePath
import qualified System.Random.Stateful as Random
import qualified Text.XML as Xml
import qualified Web.Cookie as Cookie

application :: Context.Context -> Wai.Application
application context request respond = do
    let
        method = Convert.utf8ToString $ Wai.requestMethod request
        path = fmap Convert.textToString $ Wai.pathInfo request
        handler = case (method, path) of
            ("GET", []) -> indexHandler
            ("GET", ["bootstrap.css"]) -> fileHandler "bootstrap.css" "text/css; charset=UTF-8"
            ("GET", ["favicon.ico"]) -> faviconHandler
            ("GET", ["github-callback"]) -> githubCallbackHandler
            ("GET", ["monadoc.svg"]) -> fileHandler "monadoc.svg" "image/svg+xml; charset=UTF-8"
            ("GET", ["monadoc.xsl"]) -> fileHandler "monadoc.xsl" "text/xsl; charset=UTF-8"
            ("GET", ["robots.txt"]) -> fileHandler "robots.txt" "text/plain; charset=UTF-8"
            _ -> notFoundHandler
    response <- handler context request
    respond response

getUser :: Context.Context -> Wai.Request -> IO (Maybe User.User)
getUser context request = do
    let
        cookies = Cookie.parseCookies . Maybe.fromMaybe ByteString.empty . lookup Http.hCookie $ Wai.requestHeaders request
    case lookup (Convert.stringToUtf8 "guid") cookies of
        Just byteString -> case fmap Guid.fromUuid $ Uuid.fromASCIIBytes byteString of
            Just guid -> Pool.withResource (Context.pool context) $ \ connection -> do
                    maybeSession <- Session.selectByGuid connection guid
                    case maybeSession of
                        Just session -> User.selectByGithubId connection $ Session.userGithubId session
                        Nothing -> pure Nothing
            _ -> pure Nothing
        _ -> pure Nothing

type Handler = Context.Context -> Wai.Request -> IO Wai.Response

indexHandler :: Handler
indexHandler context request = do
    let
        config = Context.config context
        baseUrl = Config.baseUrl config
        clientId = Config.clientId config
    maybeUser <- getUser context request
    pure . Response.xml Http.ok200 [] $ Xml.Document
        (Xml.Prologue
            [Xml.MiscInstruction $ Xml.Instruction
                (Convert.stringToText "xml-stylesheet")
                (Convert.stringToText $ "type=\"text/xsl\" charset=\"UTF-8\" href=\"" <> Xml.escape baseUrl <> "/monadoc.xsl\"")]
            Nothing
            [])
        (Xml.element "monadoc" []
            [ Xml.node "config" []
                [ Xml.node "baseUrl" [] [Xml.content baseUrl]
                , Xml.node "clientId" [] [Xml.content clientId]
                , Xml.node "version" [] [Xml.content $ Convert.versionToString Package.version]
                ]
            , Xml.node "user" [] $ case maybeUser of
                Nothing -> []
                Just user ->  [Xml.node "login" [] [Xml.content $ User.githubLogin user]]
            ])
        []

fileHandler :: FilePath -> String -> Handler
fileHandler relative mime context _ = do
    let
        status = Http.ok200
        headers = [(Http.hContentType, Convert.stringToUtf8 mime)]
        config = Context.config context
        directory = Config.dataDirectory config
        absolute = FilePath.combine directory relative
    Response.file status headers absolute

faviconHandler :: Handler
faviconHandler context _ = do
    let
        status = Http.found302
        config = Context.config context
        baseUrl = Config.baseUrl config
        location = Convert.stringToUtf8 $ baseUrl <> "/monadoc.svg"
        headers = [(Http.hLocation, location)]
    pure $ Response.status status headers

githubCallbackHandler :: Handler
githubCallbackHandler context request = do
    let
        config = Context.config context
        baseUrl = Config.baseUrl config
        clientId = Config.clientId config
        clientSecret = Config.clientSecret config
        manager = Context.manager context
    case lookup (Convert.stringToUtf8 "code") $ Wai.queryString request of
        Just (Just code) -> do
            accessToken <- do
                initial <- Client.parseUrlThrow "https://github.com/login/oauth/access_token"
                let
                    body =
                        [ (Convert.stringToUtf8 "client_id", Convert.stringToUtf8 clientId)
                        , (Convert.stringToUtf8 "client_secret", Convert.stringToUtf8 clientSecret)
                        , (Convert.stringToUtf8 "code", code)
                        ]
                    headers = (Http.hAccept, Convert.stringToUtf8 "application/json") : Client.requestHeaders initial
                    req = Client.urlEncodedBody body initial { Client.requestHeaders = headers }
                res <- Client.httpLbs req manager
                case Aeson.eitherDecode $ Client.responseBody res of
                    Left message -> Exception.throwM $ InvalidJson.InvalidJson message
                    Right oAuthResponse -> pure $ OAuthResponse.accessToken oAuthResponse
            githubUser <- do
                initial <- Client.parseUrlThrow "https://api.github.com/user"
                let
                    headers
                        = (Http.hAuthorization, Convert.stringToUtf8 $ "Bearer " <> accessToken)
                        : (Http.hUserAgent, Settings.serverName)
                        : Client.requestHeaders initial
                    req = initial { Client.requestHeaders = headers }
                res <- Client.httpLbs req manager
                case Aeson.eitherDecode $ Client.responseBody res of
                    Left message -> Exception.throwM $ InvalidJson.InvalidJson message
                    Right githubUser -> pure githubUser
            now <- Time.getCurrentTime
            guid <- uniformIO
            let
                user = User.User
                    { User.createdAt = now
                    , User.deletedAt = Nothing
                    , User.githubId = GithubUser.id_ githubUser
                    , User.githubLogin = GithubUser.login githubUser
                    , User.githubToken = accessToken
                    , User.updatedAt = now
                    }
                session = Session.Session
                    { Session.createdAt = now
                    , Session.deletedAt = Nothing
                    , Session.guid = guid
                    , Session.userAgent = Convert.utf8ToString . Maybe.fromMaybe ByteString.empty $ Wai.requestHeaderUserAgent request
                    , Session.userGithubId = User.githubId user
                    }
            Pool.withResource (Context.pool context) $ \ connection -> do
                User.insertOrUpdate connection user
                Session.insert connection session
            let
                cookie = Cookie.defaultSetCookie
                    { Cookie.setCookieHttpOnly = True
                    , Cookie.setCookieName = Convert.stringToUtf8 "guid"
                    , Cookie.setCookiePath = Just $ Convert.stringToUtf8 "/"
                    , Cookie.setCookieSameSite = Just Cookie.sameSiteLax
                    , Cookie.setCookieSecure = Config.isSecure config
                    , Cookie.setCookieValue = Uuid.toASCIIBytes $ Guid.toUuid guid
                    }
            pure $ Response.status Http.found302
                [ (Http.hLocation, Convert.stringToUtf8 $ baseUrl <> "/")
                , (Http.hSetCookie, LazyByteString.toStrict . Builder.toLazyByteString $ Cookie.renderSetCookie cookie)
                ]
        _ -> Exception.throwM $ MissingCode.MissingCode request

notFoundHandler :: Handler
notFoundHandler _ _ = pure $ Response.status Http.notFound404 []

uniformIO :: Random.Uniform a => IO a
uniformIO = Random.getStdRandom Random.uniform
