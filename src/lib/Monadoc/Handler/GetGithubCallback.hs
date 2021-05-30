module Monadoc.Handler.GetGithubCallback where

import Monadoc.Prelude

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Builder as Builder
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
    case lookup (into @ByteString "code") <| Wai.queryString request of
        Just (Just code) -> do
            accessToken <- do
                initial <- Client.parseUrlThrow "https://github.com/login/oauth/access_token"
                let
                    body =
                        [ (into @ByteString "client_id", into @ByteString clientId)
                        , (into @ByteString "client_secret", into @ByteString clientSecret)
                        , (into @ByteString "code", code)
                        ]
                    headers = (Http.hAccept, into @ByteString "application/json") : Client.requestHeaders initial
                    req = Client.urlEncodedBody body initial { Client.requestHeaders = headers }
                res <- Client.httpLbs req manager
                let responseBody = Client.responseBody res
                case Aeson.eitherDecode responseBody of
                    Left message -> throwM <| InvalidJson.new responseBody message
                    Right oAuthResponse -> pure <| OAuthResponse.accessToken oAuthResponse
            githubUser <- do
                initial <- Client.parseUrlThrow "https://api.github.com/user"
                let
                    headers
                        = (Http.hAuthorization, into @ByteString <| "Bearer " <> accessToken)
                        : (Http.hUserAgent, Settings.serverName)
                        : Client.requestHeaders initial
                    req = initial { Client.requestHeaders = headers }
                res <- Client.httpLbs req manager
                let responseBody = Client.responseBody res
                case Aeson.eitherDecode responseBody of
                    Left message -> throwM <| InvalidJson.new responseBody message
                    Right githubUser -> pure githubUser
            now <- Time.getCurrentTime
            guid <- Guid.random
            let
                user = User.User
                    { User.createdAt = now
                    , User.deletedAt = Nothing
                    , User.githubId = GithubUser.id githubUser
                    , User.githubLogin = GithubUser.login githubUser
                    , User.githubToken = accessToken
                    , User.updatedAt = now
                    }
                session = Session.Session
                    { Session.createdAt = now
                    , Session.deletedAt = Nothing
                    , Session.guid = guid
                    , Session.updatedAt = now
                    , Session.userAgent = maybe "" (unsafeInto @String) <| Wai.requestHeaderUserAgent request
                    , Session.userGithubId = User.githubId user
                    }
            Pool.withResource (Context.pool context) <| \ connection -> do
                User.insertOrUpdate connection user
                Session.insert connection session
            let
                cookie = Cookie.defaultSetCookie
                    { Cookie.setCookieHttpOnly = True
                    , Cookie.setCookieName = into @ByteString "guid"
                    , Cookie.setCookiePath = Just <. into @ByteString <| Route.toString Route.Index
                    , Cookie.setCookieSameSite = Just Cookie.sameSiteLax
                    , Cookie.setCookieSecure = Config.isSecure config
                    , Cookie.setCookieValue = Uuid.toASCIIBytes <| into @Uuid.UUID guid
                    }
                -- TODO: Redirect to where the user was originally.
                location = into @ByteString <| baseUrl <> Route.toString Route.Index
            pure <| Response.status Http.found302
                [ (Http.hLocation, location)
                , (Http.hSetCookie, into @ByteString <. Builder.toLazyByteString <| Cookie.renderSetCookie cookie)
                ]
        _ -> throwM <| MissingCode.new request
