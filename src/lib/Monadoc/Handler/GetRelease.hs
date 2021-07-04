module Monadoc.Handler.GetRelease where

import Monadoc.Prelude

import qualified Monadoc.Class.ToXml as ToXml
import qualified Monadoc.Exception.Found as Found
import qualified Monadoc.Exception.NotFound as NotFound
import qualified Monadoc.Handler.Common as Common
import qualified Monadoc.Model.Package as Package
import qualified Monadoc.Model.User as User
import qualified Monadoc.Type.Breadcrumb as Breadcrumb
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Handler as Handler
import qualified Monadoc.Type.Meta as Meta
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Type.PackageName as PackageName
import qualified Monadoc.Type.Release as Release
import qualified Monadoc.Type.Root as Root
import qualified Monadoc.Type.Route as Route
import qualified Monadoc.Utility.Foldable as Foldable
import qualified Monadoc.Utility.Xml as Xml

handler
    :: PackageName.PackageName
    -> Release.Release
    -> Handler.Handler
handler packageName release context request = do
    let version = Release.version release
    revision <- case Release.revision release of
        Just revision -> pure revision
        Nothing -> do
            packages <- Context.withConnection context $ \ connection ->
                Package.selectByNameAndVersion connection packageName version
            case Foldable.maximum $ fmap (Package.revision . Model.value) packages of
                Nothing -> throwM NotFound.new
                Just revision -> throwM
                    . Found.new
                    . Route.toString
                    $ Route.Release packageName release { Release.revision = Just revision }

    -- TODO: Add everything that's currently on the "get revision" endpoint.
    _package <- do
        maybePackage <- Context.withConnection context $ \ connection ->
            Package.select connection packageName version revision
        maybe (throwM NotFound.new) pure maybePackage

    maybeUser <- Common.getUser context request
    let route = Route.Release packageName release
    pure $ Common.makeResponse Root.Root
        { Root.meta = (Meta.fromContext context route)
            { Meta.breadcrumbs =
                [ Breadcrumb.Breadcrumb
                    { Breadcrumb.name = "Home"
                    , Breadcrumb.route = Just Route.Index
                    }
                , Breadcrumb.Breadcrumb
                    { Breadcrumb.name = into @String packageName
                    , Breadcrumb.route = Just $ Route.Package packageName
                    }
                , Breadcrumb.Breadcrumb
                    { Breadcrumb.name = into @String release
                    , Breadcrumb.route = Nothing
                    }
                ]
            , Meta.user = fmap (User.githubLogin . Model.value) maybeUser
            }
        , Root.page = Xml.node "release" []
            [ Xml.node "package" [] [ToXml.toXml packageName]
            , Xml.node "release" [] [ToXml.toXml release]
            ]
        }
