module Monadoc.Handler.PostRevoke where

import Monadoc.Prelude

import qualified Data.Pool as Pool
import qualified Monadoc.Handler.Common as Common
import qualified Monadoc.Model.Session as Session
import qualified Monadoc.Model.User as User
import qualified Monadoc.Server.Response as Response
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Guid as Guid
import qualified Monadoc.Type.Handler as Handler
import qualified Monadoc.Type.Route as Route
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai

handler :: Handler.Handler
handler context request = do
    maybeUser <- Common.getUser context request
    case maybeUser of
        Nothing -> pure $ Response.status Http.forbidden403 []
        Just user -> do
            body <- Wai.lazyRequestBody request
            let query = Http.parseQueryText $ into @ByteString body
            case lookup (into @Text "guid") query of
                Just (Just rawGuid) -> do
                    guid <- either throwM pure $ tryInto @Guid.Guid rawGuid
                    maybeSession <- Pool.withResource (Context.pool context) $ \ connection ->
                        Session.selectByGuid connection guid
                    case maybeSession of
                        Nothing -> pure $ Response.status Http.notFound404 []
                        Just session -> if Session.userGithubId session == User.githubId user
                            then do
                                Pool.withResource (Context.pool context) $ \ connection ->
                                    Session.delete connection session
                                let
                                    config = Context.config context
                                    baseUrl = Config.baseUrl config
                                    location = into @ByteString $ baseUrl <> Route.toString Route.Account
                                pure $ Response.status Http.found302 [(Http.hLocation, location)]
                            else pure $ Response.status Http.forbidden403 []
                _ -> pure $ Response.status Http.notFound404 []
