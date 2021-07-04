module Monadoc.Handler.GetVersion where

import Monadoc.Prelude

import qualified Monadoc.Exception.Found as Found
import qualified Monadoc.Exception.NotFound as NotFound
import qualified Monadoc.Model.Package as Package
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Handler as Handler
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Type.PackageName as PackageName
import qualified Monadoc.Type.Release as Release
import qualified Monadoc.Type.Route as Route
import qualified Monadoc.Type.Version as Version
import qualified Monadoc.Utility.Foldable as Foldable

handler :: PackageName.PackageName -> Version.Version -> Handler.Handler
handler packageName version context _ = do
    packages <- Context.withConnection context $ \ connection ->
        Package.selectByNameAndVersion connection packageName version
    package <- packages
        & fmap Model.value
        & Foldable.maximumOn Package.revision
        & maybe (throwM NotFound.new) pure
    let
        config = Context.config context
        baseUrl = Config.baseUrl config
        route = Route.Release packageName Release.Release
            { Release.version
            , Release.revision = Just $ Package.revision package
            }
        location = baseUrl <> Route.toString route
    throwM $ Found.new location
