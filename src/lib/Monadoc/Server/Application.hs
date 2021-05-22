module Monadoc.Server.Application where

import qualified Control.Monad.Catch as Exception
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Pool as Pool
import qualified Data.Time as Time
import qualified Database.SQLite.Simple as Sql
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
import qualified Network.Wai as Wai
import qualified Paths_monadoc as Package
import qualified System.FilePath as FilePath
import qualified Text.XML as Xml

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
    case (method, path) of
        ("GET", []) -> respond . Response.xml Http.ok200 [] $ Xml.Document
            (Xml.Prologue
                [Xml.MiscInstruction $ Xml.Instruction
                    (Convert.stringToText "xml-stylesheet")
                    (Convert.stringToText $ "type=\"text/xsl\" charset=\"UTF-8\" href=\"" <> Xml.escape baseUrl <> "/monadoc.xsl\"")]
                Nothing
                [])
            (Xml.element "monadoc" [] [])
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
                    let
                        user = User
                            { userCreatedAt = now
                            , userGithubId = githubUserId githubUser
                            , userGithubLogin = githubUserLogin githubUser
                            , userGithubToken = accessToken
                            , userUpdatedAt = now
                            }
                    Pool.withResource (Context.pool context) $ \ connection ->
                        Sql.execute
                            connection
                            (Convert.stringToQuery "insert into user \
                            \ ( createdAt \
                            \ , githubId \
                            \ , githubLogin \
                            \ , githubToken \
                            \ , updatedAt \
                            \ ) values (?, ?, ?, ?, ?) \
                            \ on conflict (githubId) do update \
                            \ set githubId = excluded.githubId \
                            \ , githubLogin = excluded.githubLogin \
                            \ , githubToken = excluded.githubToken \
                            \ , updatedAt = excluded.updatedAt")
                            [ Sql.toField $ userCreatedAt user
                            , Sql.toField $ userGithubId user
                            , Sql.toField $ userGithubLogin user
                            , Sql.toField $ userGithubToken user
                            , Sql.toField $ userUpdatedAt user
                            ]
                    respond $ Response.status Http.found302 [(Http.hLocation, Convert.stringToUtf8 $ baseUrl <> "/")]
                _ -> Exception.throwM $ MissingCode.MissingCode request
        ("GET", ["monadoc.svg"]) -> do
            contents <- LazyByteString.readFile $ FilePath.combine dataDirectory "monadoc.svg"
            respond $ Response.lazyByteString Http.ok200 [(Http.hContentType, Convert.stringToUtf8 "image/svg+xml; charset=UTF-8")] contents
        ("GET", ["monadoc.xsl"]) -> respond . Response.xml Http.ok200 [] $ Xml.Document
            (Xml.Prologue [] Nothing [])
            (Xml.element "xsl:stylesheet" [("version", "1.0")]
                [ Xml.node "xsl:output"
                    [ ("method", "html")
                    , ("media-type", "text/html")
                    , ("encoding", "UTF-8")
                    -- https://stackoverflow.com/a/3404922/1274282
                    , ("doctype-system", "about:legacy-compat")
                    ] []
                , Xml.node "xsl:variable" [("name", "base-url")] [Xml.content baseUrl]
                , Xml.node "xsl:template" [("match", "/")]
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
                                                [ Xml.node "a"
                                                    [ ("class", "nav-link")
                                                    , ("href", "https://github.com/login/oauth/authorize?client_id=" <> clientId <> "&redirect_uri={$base-url}/github-callback")
                                                    ] [Xml.content "Log in"]
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
    , userGithubId :: Int
    , userGithubLogin :: String
    , userGithubToken :: String
    , userUpdatedAt :: Time.UTCTime
    } deriving (Eq, Show)
