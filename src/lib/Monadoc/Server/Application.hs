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
import qualified Database.SQLite.Simple as Sql
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
        config = Context.config context
        dataDirectory = Config.dataDirectory config
        baseUrl = Config.baseUrl config
        clientId = Config.clientId config
        clientSecret = Config.clientSecret config
        manager = Context.manager context
        cookies = Cookie.parseCookies . Maybe.fromMaybe ByteString.empty . lookup Http.hCookie $ Wai.requestHeaders request
    maybeUser <- case lookup (Convert.stringToUtf8 "guid") cookies of
        Just byteString -> case fmap Guid.fromUuid $ Uuid.fromASCIIBytes byteString of
            Just guid -> Pool.withResource (Context.pool context) $ \ connection -> do
                    sessions <- Sql.query connection (Convert.stringToQuery "select createdAt, deletedAt, guid, userAgent, userGithubId from session where guid = ? and deletedAt is null limit 1") [guid]
                    case sessions of
                        session : _ -> do
                            users <- Sql.query connection (Convert.stringToQuery "select createdAt, deletedAt, githubId, githubLogin, githubToken, updatedAt from user where githubId = ? and deletedAt is null limit 1") [Session.userGithubId session]
                            case users of
                                user : _ -> pure $ Just user
                                _ -> pure Nothing
                        _ -> pure Nothing
            _ -> pure Nothing
        _ -> pure Nothing
    case (method, path) of
        ("GET", []) -> respond . Response.xml Http.ok200 [] $ Xml.Document
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
        ("GET", ["bootstrap.css"]) -> do
            contents <- LazyByteString.readFile $ FilePath.combine dataDirectory "bootstrap.css"
            respond $ Response.lazyByteString Http.ok200 [(Http.hContentType, Convert.stringToUtf8 "text/css; charset=UTF-8")] contents
        ("GET", ["favicon.ico"]) -> respond $ Response.status Http.found302
            [ (Http.hLocation, Convert.stringToUtf8 $ baseUrl <> "/monadoc.svg")
            ]
        ("GET", ["github-callback"]) ->
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
                        Sql.execute
                            connection
                            (Convert.stringToQuery "insert into user \
                            \ ( createdAt \
                            \ , deletedAt \
                            \ , githubId \
                            \ , githubLogin \
                            \ , githubToken \
                            \ , updatedAt \
                            \ ) values (?, ?, ?, ?, ?, ?) \
                            \ on conflict (githubId) do update \
                            \ set deletedAt = excluded.deletedAt \
                            \ , githubLogin = excluded.githubLogin \
                            \ , githubToken = excluded.githubToken \
                            \ , updatedAt = excluded.updatedAt")
                            user
                        Sql.execute
                            connection
                            (Convert.stringToQuery "insert into session \
                            \ ( createdAt, deletedAt, guid, userAgent, userGithubId )\
                            \ values ( ?, ?, ?, ?, ? )")
                            session
                    let
                        cookie = Cookie.defaultSetCookie
                            { Cookie.setCookieHttpOnly = True
                            , Cookie.setCookieName = Convert.stringToUtf8 "guid"
                            , Cookie.setCookiePath = Just $ Convert.stringToUtf8 "/"
                            , Cookie.setCookieSameSite = Just Cookie.sameSiteLax
                            , Cookie.setCookieSecure = Config.isSecure config
                            , Cookie.setCookieValue = Uuid.toASCIIBytes $ Guid.toUuid guid
                            }
                    respond $ Response.status Http.found302
                        [ (Http.hLocation, Convert.stringToUtf8 $ baseUrl <> "/")
                        , (Http.hSetCookie, LazyByteString.toStrict . Builder.toLazyByteString $ Cookie.renderSetCookie cookie)
                        ]
                _ -> Exception.throwM $ MissingCode.MissingCode request
        ("GET", ["monadoc.svg"]) -> do
            contents <- LazyByteString.readFile $ FilePath.combine dataDirectory "monadoc.svg"
            respond $ Response.lazyByteString Http.ok200 [(Http.hContentType, Convert.stringToUtf8 "image/svg+xml; charset=UTF-8")] contents
        ("GET", ["monadoc.xsl"]) -> do
            contents <- LazyByteString.readFile $ FilePath.combine dataDirectory "monadoc.xsl"
            respond $ Response.lazyByteString Http.ok200 [(Http.hContentType, Convert.stringToUtf8 "text/xsl; charset=UTF-8")] contents
        ("GET", ["robots.txt"]) -> respond . Response.string Http.ok200 [] $ unlines ["User-Agent: *", "Allow: /"]
        _ -> respond $ Response.status Http.notFound404 []

uniformIO :: Random.Uniform a => IO a
uniformIO = Random.getStdRandom Random.uniform
