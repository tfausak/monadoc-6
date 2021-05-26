module Monadoc.Handler.GetGithubCallback where

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
import qualified Monadoc.Type.Handler as Handler
import qualified Monadoc.Type.OAuthResponse as OAuthResponse
import qualified Monadoc.Type.Route as Route
import qualified Monadoc.Utility.Convert as Convert
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Types as Http
import qualified Network.HTTP.Types.Header as Http
import qualified Network.Wai as Wai
import qualified Web.Cookie as Cookie

handler :: Handler.Handler
handler context request = do
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
            guid <- Guid.random
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
                    , Session.updatedAt = now
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
                    , Cookie.setCookiePath = Just . Convert.stringToUtf8 $ Route.toString Route.Index
                    , Cookie.setCookieSameSite = Just Cookie.sameSiteLax
                    , Cookie.setCookieSecure = Config.isSecure config
                    , Cookie.setCookieValue = Uuid.toASCIIBytes $ Guid.toUuid guid
                    }
            pure $ Response.status Http.found302
                [ (Http.hLocation, Convert.stringToUtf8 $ baseUrl <> Route.toString Route.Index)
                , (Http.hSetCookie, LazyByteString.toStrict . Builder.toLazyByteString $ Cookie.renderSetCookie cookie)
                ]
        _ -> Exception.throwM $ MissingCode.MissingCode request
