{-# LANGUAGE TypeApplications #-}

module Monadoc.Handler.PostRevoke where

import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Exception
import qualified Data.ByteString as ByteString
import qualified Data.Text as Text
import qualified Monadoc.Exception.Forbidden as Forbidden
import qualified Monadoc.Exception.Found as Found
import qualified Monadoc.Exception.NotFound as NotFound
import qualified Monadoc.Handler.Common as Common
import qualified Monadoc.Model.Session as Session
import qualified Monadoc.Model.User as User
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Guid as Guid
import qualified Monadoc.Type.Handler as Handler
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Type.Route as Route
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Witch

handler :: Handler.Handler
handler context request = do
    maybeUser <- Common.getUser context request
    user <- case maybeUser of
        Nothing -> Exception.throwM Forbidden.new
        Just user -> pure user
    body <- Wai.lazyRequestBody request
    let query = Http.parseQueryText $ Witch.into @ByteString.ByteString body
    rawGuid <- case lookup (Witch.into @Text.Text "guid") query of
        Just (Just rawGuid) -> pure rawGuid
        _ -> Exception.throwM NotFound.new
    guid <- either Exception.throwM pure $ Witch.tryInto @Guid.Guid rawGuid
    maybeSession <- Context.withConnection context $ \ connection ->
        Session.selectByGuid connection guid
    session <- case maybeSession of
        Nothing -> Exception.throwM NotFound.new
        Just session -> pure session
    Monad.when (Session.userGithubId (Model.value session) /= User.githubId (Model.value user)) $ Exception.throwM Forbidden.new
    Context.withConnection context $ \ connection ->
        Session.delete connection $ Model.key session
    let
        config = Context.config context
        baseUrl = Config.baseUrl config
        location = baseUrl <> Route.toString Route.Account
    Exception.throwM $ Found.new location
