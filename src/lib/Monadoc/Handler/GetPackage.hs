module Monadoc.Handler.GetPackage where

import Monadoc.Prelude

import qualified Data.Pool as Pool
import qualified Monadoc.Exception.NotFound as NotFound
import qualified Monadoc.Model.Package as Package
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Handler as Handler
import qualified Monadoc.Type.PackageName as PackageName
import qualified Monadoc.Type.Route as Route
import qualified Monadoc.Utility.Foldable as Foldable
import qualified Network.HTTP.Types as Http
import qualified Monadoc.Server.Response as Response
import qualified Monadoc.Type.Config as Config

handler :: PackageName.PackageName -> Handler.Handler
handler packageName context _ = do
    packages <- Pool.withResource (Context.pool context) $ \ connection ->
        Package.selectByName connection packageName
    package <- case Foldable.maximumOn (\ p -> (Package.version p, Package.revision p)) packages of
        Nothing -> throwM NotFound.new
        Just package -> pure package
    let
        config = Context.config context
        baseUrl = Config.baseUrl config
        version = Package.version package
        revision = Package.revision package
        route = Route.Revision packageName version revision
        location = into @ByteString $ baseUrl <> Route.toString route
    pure $ Response.status Http.found302 [(Http.hLocation, location)]
