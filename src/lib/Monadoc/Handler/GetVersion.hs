module Monadoc.Handler.GetVersion where

import Monadoc.Prelude

import qualified Data.Pool as Pool
import qualified Monadoc.Exception.NotFound as NotFound
import qualified Monadoc.Model.Package as Package
import qualified Monadoc.Server.Response as Response
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Handler as Handler
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Type.PackageName as PackageName
import qualified Monadoc.Type.Route as Route
import qualified Monadoc.Type.Version as Version
import qualified Monadoc.Utility.Foldable as Foldable
import qualified Network.HTTP.Types as Http

handler :: PackageName.PackageName -> Version.Version -> Handler.Handler
handler packageName version context _ = do
    packages <- Pool.withResource (Context.pool context) $ \ connection ->
        Package.selectByNameAndVersion connection packageName version
    package <- case Foldable.maximumOn Package.revision $ fmap Model.value packages of
        Nothing -> throwM NotFound.new
        Just package -> pure package
    let
        config = Context.config context
        baseUrl = Config.baseUrl config
        route = Route.Revision packageName version $ Package.revision package
        location = into @ByteString $ baseUrl <> Route.toString route
    pure $ Response.status Http.found302 [(Http.hLocation, location)]
