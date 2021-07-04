module Monadoc.Handler.GetPackage where

import Monadoc.Prelude

import qualified Monadoc.Exception.Found as Found
import qualified Monadoc.Exception.NotFound as NotFound
import qualified Monadoc.Model.Package as Package
import qualified Monadoc.Model.PreferredVersions as PreferredVersions
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Handler as Handler
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Type.PackageName as PackageName
import qualified Monadoc.Type.Release as Release
import qualified Monadoc.Type.Route as Route
import qualified Monadoc.Type.VersionRange as VersionRange
import qualified Monadoc.Utility.Foldable as Foldable

handler :: PackageName.PackageName -> Handler.Handler
handler packageName context _ = do
    packages <- Context.withConnection context $ \ connection ->
        Package.selectByName connection packageName
    -- TODO: Use latest version rather than preferred versions.
    maybePreferredVersions <- Context.withConnection context $ \ connection ->
        PreferredVersions.selectByPackageName connection packageName
    let versionRange = maybe VersionRange.any (PreferredVersions.versionRange . Model.value) maybePreferredVersions
    package <- packages
        & fmap Model.value
        & Foldable.maximumOn (\ x ->
            ( VersionRange.contains (Package.version x) versionRange
            , Package.version x
            , Package.revision x
            ))
        & maybe (throwM NotFound.new) pure
    let
        config = Context.config context
        baseUrl = Config.baseUrl config
        version = Package.version package
        revision = Package.revision package
        route = Route.Release packageName Release.Release
            { Release.version
            , Release.revision = Just revision
            }
        location = baseUrl <> Route.toString route
    throwM $ Found.new location
