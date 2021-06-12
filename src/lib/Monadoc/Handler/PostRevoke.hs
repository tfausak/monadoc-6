module Monadoc.Handler.PostRevoke where

import Monadoc.Prelude

import qualified Data.Pool as Pool
import qualified Monadoc.Exception.Forbidden as Forbidden
import qualified Monadoc.Exception.NotFound as NotFound
import qualified Monadoc.Handler.Common as Common
import qualified Monadoc.Model.Session as Session
import qualified Monadoc.Model.User as User
import qualified Monadoc.Server.Response as Response
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Guid as Guid
import qualified Monadoc.Type.Handler as Handler
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Type.Route as Route
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai

handler :: Handler.Handler
handler context request = do
    maybeUser <- Common.getUser context request
    user <- case maybeUser of
        Nothing -> throwM Forbidden.new
        Just user -> pure user
    body <- Wai.lazyRequestBody request
    let query = Http.parseQueryText $ into @ByteString body
    rawGuid <- case lookup (into @Text "guid") query of
        Just (Just rawGuid) -> pure rawGuid
        _ -> throwM NotFound.new
    guid <- either throwM pure $ tryInto @Guid.Guid rawGuid
    maybeSession <- Pool.withResource (Context.pool context) $ \ connection ->
        Session.selectByGuid connection guid
    session <- case maybeSession of
        Nothing -> throwM NotFound.new
        Just session -> pure session
    when (Session.userGithubId (Model.value session) /= User.githubId user) $ throwM Forbidden.new
    Pool.withResource (Context.pool context) $ \ connection ->
        Session.delete connection $ Model.key session
    let
        config = Context.config context
        baseUrl = Config.baseUrl config
        location = into @ByteString $ baseUrl <> Route.toString Route.Account
    pure $ Response.status Http.found302 [(Http.hLocation, location)]
