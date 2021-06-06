module Monadoc.Handler.GetPackage where

import Monadoc.Prelude

import qualified Data.List as List
import qualified Data.Ord as Ord
import qualified Data.Pool as Pool
import qualified Monadoc.Class.ToXml as ToXml
import qualified Monadoc.Exception.NotFound as NotFound
import qualified Monadoc.Handler.Common as Common
import qualified Monadoc.Model.Package as Package
import qualified Monadoc.Model.User as User
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Handler as Handler
import qualified Monadoc.Type.PackageName as PackageName
import qualified Monadoc.Type.Route as Route
import qualified Monadoc.Type.Version as Version
import qualified Monadoc.Utility.Xml as Xml

handler :: PackageName.PackageName -> Handler.Handler
handler packageName context request = do
    maybeUser <- Common.getUser context request
    packages <- Pool.withResource (Context.pool context) $ \ connection ->
        Package.selectByName connection packageName
    when (null packages) $ throwM NotFound.new
    pure $ Common.makeResponse Common.Monadoc
        { Common.monadoc_config = (Common.config_fromContext context)
            { Common.config_breadcrumbs =
                [ Common.Breadcrumb
                    { Common.breadcrumb_name = "Home"
                    , Common.breadcrumb_route = Just Route.Index
                    }
                , Common.Breadcrumb
                    { Common.breadcrumb_name = into @String packageName
                    , Common.breadcrumb_route = Nothing
                    }
                ]
            , Common.config_user = fmap User.githubLogin maybeUser
            }
        , Common.monadoc_page = Package
            { package_name = packageName
            , package_versions =
                fmap (\ package -> Version
                    { version_number = Package.version package
                    , version_route = Route.Version (Package.name package) (Package.version package)
                    })
                $ List.sortOn (Ord.Down . Package.version) packages
            }
        }

data Package = Package
    { package_name :: PackageName.PackageName
    , package_versions :: [Version]
    } deriving (Eq, Show)

instance ToXml.ToXml Package where
    toXml package = Xml.node "package" []
        [ Xml.node "name" [] [ToXml.toXml $ package_name package]
        , Xml.node "versions" [] . fmap ToXml.toXml $ package_versions package
        ]

data Version = Version
    { version_number :: Version.Version
    , version_route :: Route.Route
    } deriving (Eq, Show)

instance ToXml.ToXml Version where
    toXml version = Xml.node "version" []
        [ Xml.node "number" [] [ToXml.toXml $ version_number version]
        , Xml.node "route" [] [ToXml.toXml $ version_route version]
        ]
