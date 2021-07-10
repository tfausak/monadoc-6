{-# LANGUAGE TypeApplications #-}

module Monadoc.Handler.PostLogOut where

import Monadoc.Prelude

import qualified Data.ByteString.Builder as Builder
import qualified Data.Time as Time
import qualified Monadoc.Exception.Forbidden as Forbidden
import qualified Monadoc.Handler.Common as Common
import qualified Monadoc.Model.Session as Session
import qualified Monadoc.Server.Response as Response
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Handler as Handler
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Type.Route as Route
import qualified Network.HTTP.Types as Http
import qualified Network.HTTP.Types.Header as Http
import qualified Web.Cookie as Cookie

handler :: Handler.Handler
handler context request = do
    maybeSession <- Common.getSession context request
    session <- case maybeSession of
        Nothing -> throwM Forbidden.new
        Just session -> pure session
    Context.withConnection context $ \ connection ->
        Session.delete connection $ Model.key session
    let
        config = Context.config context
        baseUrl = Config.baseUrl config
        location = into @ByteString $ baseUrl <> Route.toString Route.Index
        epoch = Time.UTCTime (Time.fromGregorian 1970 1 1) 0
        cookie = Cookie.defaultSetCookie
            { Cookie.setCookieExpires = Just epoch
            , Cookie.setCookieHttpOnly = True
            , Cookie.setCookieName = into @ByteString "guid"
            , Cookie.setCookiePath = Just . into @ByteString $ Route.toString Route.Index
            , Cookie.setCookieSameSite = Just Cookie.sameSiteLax
            , Cookie.setCookieSecure = Config.isSecure config
            , Cookie.setCookieValue = into @ByteString ""
            }
    pure $ Response.status Http.found302
        [ (Http.hLocation, location)
        , (Http.hSetCookie, into @ByteString . Builder.toLazyByteString $ Cookie.renderSetCookie cookie)
        ]
