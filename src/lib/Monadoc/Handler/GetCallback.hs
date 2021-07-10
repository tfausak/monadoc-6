{-# LANGUAGE TypeApplications #-}

module Monadoc.Handler.GetCallback where

import Monadoc.Prelude

import qualified Control.Monad.Catch as Exception
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Builder as Builder
import qualified Data.Time as Time
import qualified Data.UUID as Uuid
import qualified Monadoc.Exception.InvalidJson as InvalidJson
import qualified Monadoc.Exception.MissingCode as MissingCode
import qualified Monadoc.Model.Session as Session
import qualified Monadoc.Model.User as User
import qualified Monadoc.Server.Response as Response
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.GithubToken as GithubToken
import qualified Monadoc.Type.GithubUser as GithubUser
import qualified Monadoc.Type.Guid as Guid
import qualified Monadoc.Type.Handler as Handler
import qualified Monadoc.Type.OAuthResponse as OAuthResponse
import qualified Monadoc.Type.Route as Route
import qualified Monadoc.Vendor.Client as Client
import qualified Network.HTTP.Types as Http
import qualified Network.HTTP.Types.Header as Http
import qualified Network.Wai as Wai
import qualified Web.Cookie as Cookie

handler :: Handler.Handler
handler context request = do
    code <- case lookup (into @ByteString.ByteString "code") $ Wai.queryString request of
        Just (Just code) -> pure code
        _ -> Exception.throwM $ MissingCode.new request
    let
        config = Context.config context
        baseUrl = Config.baseUrl config
    accessToken <- getAccessToken context code
    githubUser <- getGithubUser context accessToken
    now <- Time.getCurrentTime
    guid <- Guid.random
    userAgent <- case Wai.requestHeaderUserAgent request of
        Nothing -> pure ""
        Just rawUserAgent -> either Exception.throwM pure $ tryInto @String rawUserAgent
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
            , Session.userAgent = userAgent
            , Session.userGithubId = User.githubId user
            }
    Context.withConnection context $ \ connection -> do
        User.insertOrUpdate connection user
        Session.insert connection session
    let
        cookie = Cookie.defaultSetCookie
            { Cookie.setCookieHttpOnly = True
            , Cookie.setCookieName = into @ByteString.ByteString "guid"
            , Cookie.setCookiePath = Just . into @ByteString.ByteString $ Route.toString Route.Index
            , Cookie.setCookieSameSite = Just Cookie.sameSiteLax
            , Cookie.setCookieSecure = Config.isSecure config
            , Cookie.setCookieValue = Uuid.toASCIIBytes $ into @Uuid.UUID guid
            }
        location = case lookup (into @ByteString.ByteString "state") $ Wai.queryString request of
            Just (Just x) -> into @ByteString.ByteString baseUrl <> x
            _ -> into @ByteString.ByteString $ baseUrl <> Route.toString Route.Index
    pure $ Response.status Http.found302
        [ (Http.hLocation, location)
        , (Http.hSetCookie, into @ByteString.ByteString . Builder.toLazyByteString $ Cookie.renderSetCookie cookie)
        ]

getAccessToken :: Context.Context -> ByteString.ByteString -> IO GithubToken.GithubToken
getAccessToken context code = do
    let
        config = Context.config context
        clientId = Config.clientId config
        clientSecret = Config.clientSecret config
        manager = Context.manager context
    initial <- Client.parseUrlThrow "https://github.com/login/oauth/access_token"
    let
        body =
            [ (into @ByteString.ByteString "client_id", into @ByteString.ByteString clientId)
            , (into @ByteString.ByteString "client_secret", into @ByteString.ByteString clientSecret)
            , (into @ByteString.ByteString "code", code)
            ]
        headers = (Http.hAccept, into @ByteString.ByteString "application/json") : Client.requestHeaders initial
        req = Client.urlEncodedBody body initial { Client.requestHeaders = headers }
    res <- Client.performRequest manager req
    let responseBody = Client.responseBody res
    case Aeson.eitherDecode responseBody of
        Left message -> Exception.throwM $ InvalidJson.new message responseBody
        Right oAuthResponse -> pure $ OAuthResponse.accessToken oAuthResponse

getGithubUser :: Context.Context -> GithubToken.GithubToken -> IO GithubUser.GithubUser
getGithubUser context accessToken = do
    let manager = Context.manager context
    initial <- Client.parseUrlThrow "https://api.github.com/user"
    let
        headers
            = (Http.hAuthorization, into @ByteString.ByteString $ "Bearer " <> into @String accessToken)
            : Client.requestHeaders initial
        req = initial { Client.requestHeaders = headers }
    res <- Client.performRequest manager req
    let responseBody = Client.responseBody res
    case Aeson.eitherDecode responseBody of
        Left message -> Exception.throwM $ InvalidJson.new message responseBody
        Right githubUser -> pure githubUser
