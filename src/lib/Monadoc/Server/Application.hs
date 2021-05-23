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
import qualified Database.SQLite.Simple.FromField as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import qualified Monadoc.Exception.InvalidJson as InvalidJson
import qualified Monadoc.Exception.MissingCode as MissingCode
import qualified Monadoc.Server.Response as Response
import qualified Monadoc.Server.Settings as Settings
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Context as Context
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
        Just byteString -> case fmap uuidToGuid $ Uuid.fromASCIIBytes byteString of
            Just guid -> Pool.withResource (Context.pool context) $ \ connection -> do
                    sessions <- Sql.query connection (Convert.stringToQuery "select createdAt, deletedAt, guid, userAgent, userGithubId from session where guid = ? and deletedAt is null limit 1") [guid]
                    case sessions of
                        session : _ -> do
                            users <- Sql.query connection (Convert.stringToQuery "select createdAt, deletedAt, githubId, githubLogin, githubToken, updatedAt from user where githubId = ? and deletedAt is null limit 1") [sessionUserGithubId session]
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
                [ Xml.node "base-url" [] [Xml.content baseUrl]
                , Xml.node "user" [] $ case maybeUser of
                    Nothing -> []
                    Just user ->  [Xml.node "login" [] [Xml.content $ userGithubLogin user]]
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
                            Right oAuthResponse -> pure $ oAuthResponseAccessToken oAuthResponse
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
                        user = User
                            { userCreatedAt = now
                            , userDeletedAt = Nothing
                            , userGithubId = githubUserId githubUser
                            , userGithubLogin = githubUserLogin githubUser
                            , userGithubToken = accessToken
                            , userUpdatedAt = now
                            }
                        session = Session
                            { sessionCreatedAt = now
                            , sessionDeletedAt = Nothing
                            , sessionGuid = guid
                            , sessionUserAgent = Convert.utf8ToString . Maybe.fromMaybe ByteString.empty $ Wai.requestHeaderUserAgent request
                            , sessionUserGithubId = userGithubId user
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
                            , Cookie.setCookieValue = Uuid.toASCIIBytes $ guidToUuid guid
                            }
                    respond $ Response.status Http.found302
                        [ (Http.hLocation, Convert.stringToUtf8 $ baseUrl <> "/")
                        , (Http.hSetCookie, LazyByteString.toStrict . Builder.toLazyByteString $ Cookie.renderSetCookie cookie)
                        ]
                _ -> Exception.throwM $ MissingCode.MissingCode request
        ("GET", ["monadoc.svg"]) -> do
            contents <- LazyByteString.readFile $ FilePath.combine dataDirectory "monadoc.svg"
            respond $ Response.lazyByteString Http.ok200 [(Http.hContentType, Convert.stringToUtf8 "image/svg+xml; charset=UTF-8")] contents
        ("GET", ["monadoc.xsl"]) -> respond . Response.xml Http.ok200 [] $ Xml.Document
            (Xml.Prologue [] Nothing [])
            -- https://www.w3.org/TR/1999/REC-xslt-19991116
            (Xml.element "xsl:stylesheet" [("version", "1.0")]
                [ Xml.node "xsl:output"
                    [ ("method", "html")
                    , ("media-type", "text/html")
                    , ("encoding", "UTF-8")
                    -- https://stackoverflow.com/a/3404922/1274282
                    , ("doctype-system", "about:legacy-compat")
                    ] []
                , Xml.node "xsl:variable" [("name", "base-url"), ("select", "normalize-space(/monadoc/base-url)")] []
                , Xml.node "xsl:template" [("match", "user")]
                    [ Xml.node "xsl:choose" []
                        [ Xml.node "xsl:when" [("test", "login")]
                            [ Xml.content "@"
                            , Xml.node "xsl:value-of" [("select", "login")] []
                            ]
                        , Xml.node "xsl:otherwise" []
                            [ Xml.node "a"
                                [ ("class", "nav-link")
                                , ("href", "https://github.com/login/oauth/authorize?client_id=" <> clientId <> "&redirect_uri={$base-url}/github-callback")
                                ] [Xml.content "Log in"]
                            ]
                        ]
                    ]
                , Xml.node "xsl:template" [("match", "/monadoc")]
                    [ Xml.node "html" [("lang", "en-US")]
                        [ Xml.node "head" []
                            [ Xml.node "meta"
                                [ ("name", "viewport")
                                , ("content", "initial-scale = 1, width = device-width")
                                ] []
                            , Xml.node "title" [] [Xml.content "Monadoc"]
                            , Xml.node "link"
                                [ ("rel", "stylesheet")
                                , ("href", "{$base-url}/bootstrap.css")
                                ] []
                            , Xml.node "link"
                                [ ("rel", "icon")
                                , ("type", "image/svg+xml")
                                , ("href", "{$base-url}/monadoc.svg")
                                ]
                                []
                            ]
                        , Xml.node "body" []
                            [ Xml.node "header" [("class", "mb-3")]
                                [ Xml.node "nav" [("class", "navbar navbar-light bg-light")]
                                    [ Xml.node "div" [("class", "container-fluid")]
                                        [ Xml.node "a" [("class", "navbar-brand"), ("href", "{$base-url}/")] [Xml.content "Monadoc"]
                                        , Xml.node "ul" [("class", "navbar-nav")]
                                            [ Xml.node "li" [("class", "nav-item")]
                                                [ Xml.node "xsl:apply-templates" [("select", "user")] []
                                                ]
                                            ]
                                        ]
                                    ]
                                ]
                            , Xml.node "main" [("class", "container-fluid mt-3 mb-3")]
                                [ Xml.node "p" [] [Xml.content "\x1f516 Better Haskell documentation."]
                                ]
                            , Xml.node "footer" [("class", "container-fluid pt-3 mt-3 text-muted border-top")]
                                [ Xml.node "p" []
                                    [ Xml.content "Powered by "
                                    , Xml.node "a" [("href", "https://github.com/tfausak/monadoc")] [Xml.content "Monadoc"]
                                    , Xml.content $ " version " <> Convert.versionToString Package.version <> "."
                                    ]
                                ]
                            ]
                        ]
                    ]
                ])
            []
        ("GET", ["robots.txt"]) -> respond . Response.string Http.ok200 [] $ unlines ["User-Agent: *", "Allow: /"]
        _ -> respond $ Response.status Http.notFound404 []

newtype OAuthResponse = OAuthResponse
    { oAuthResponseAccessToken :: String
    } deriving (Eq, Show)

instance Aeson.FromJSON OAuthResponse where
    parseJSON = Aeson.withObject "OAuthResponse" $ \ object -> do
        accessToken <- object Aeson..: Convert.stringToText "access_token"
        pure $ OAuthResponse accessToken

data GithubUser = GithubUser
    { githubUserId :: Int
    , githubUserLogin :: String
    } deriving (Eq, Show)

instance Aeson.FromJSON GithubUser where
    parseJSON = Aeson.withObject "GithubUser" $ \ object -> do
        id_ <- object Aeson..: Convert.stringToText "id"
        login <- object Aeson..: Convert.stringToText "login"
        pure GithubUser
            { githubUserId = id_
            , githubUserLogin = login
            }

data User = User
    { userCreatedAt :: Time.UTCTime
    , userDeletedAt :: Maybe Time.UTCTime
    , userGithubId :: Int
    , userGithubLogin :: String
    , userGithubToken :: String
    , userUpdatedAt :: Time.UTCTime
    } deriving (Eq, Show)

instance Sql.FromRow User where
    fromRow = User
        <$> Sql.field
        <*> Sql.field
        <*> Sql.field
        <*> Sql.field
        <*> Sql.field
        <*> Sql.field

instance Sql.ToRow User where
    toRow user =
        [ Sql.toField $ userCreatedAt user
        , Sql.toField $ userDeletedAt user
        , Sql.toField $ userGithubId user
        , Sql.toField $ userGithubLogin user
        , Sql.toField $ userGithubToken user
        , Sql.toField $ userUpdatedAt user
        ]

newtype Guid
    = Guid Uuid.UUID
    deriving (Eq, Show)

instance Sql.FromField Guid where
    fromField field = do
        text <- Sql.fromField field
        case Uuid.fromText text of
            Nothing -> Sql.returnError Sql.ConversionFailed field "invalid Guid"
            Just uuid -> pure $ uuidToGuid uuid

instance Sql.ToField Guid where
    toField = Sql.toField . Uuid.toText . guidToUuid

instance Random.Uniform Guid where
    uniformM = fmap uuidToGuid . Random.uniformM

uuidToGuid :: Uuid.UUID -> Guid
uuidToGuid = Guid

guidToUuid :: Guid -> Uuid.UUID
guidToUuid (Guid x) = x

uniformIO :: Random.Uniform a => IO a
uniformIO = Random.getStdRandom Random.uniform

data Session = Session
    { sessionCreatedAt :: Time.UTCTime
    , sessionDeletedAt :: Maybe Time.UTCTime
    , sessionGuid :: Guid
    , sessionUserAgent :: String
    , sessionUserGithubId :: Int
    } deriving (Eq, Show)

instance Sql.FromRow Session where
    fromRow = Session
        <$> Sql.field
        <*> Sql.field
        <*> Sql.field
        <*> Sql.field
        <*> Sql.field

instance Sql.ToRow Session where
    toRow session =
        [ Sql.toField $ sessionCreatedAt session
        , Sql.toField $ sessionDeletedAt session
        , Sql.toField $ sessionGuid session
        , Sql.toField $ sessionUserAgent session
        , Sql.toField $ sessionUserGithubId session
        ]
