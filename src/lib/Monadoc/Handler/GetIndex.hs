module Monadoc.Handler.GetIndex where

import Monadoc.Prelude

import qualified Data.Pool as Pool
import qualified Monadoc.Class.ToXml as ToXml
import qualified Monadoc.Handler.Common as Common
import qualified Monadoc.Model.Package as Package
import qualified Monadoc.Model.User as User
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Handler as Handler
import qualified Monadoc.Type.PackageName as PackageName
import qualified Monadoc.Type.Route as Route
import qualified Monadoc.Utility.Xml as Xml

handler :: Handler.Handler
handler context request = do
    maybeUser <- Common.getUser context request
    packages <- Pool.withResource (Context.pool context) Package.selectRecent
    pure $ Common.makeResponse Common.Monadoc
        { Common.monadoc_config = (Common.config_fromContext context)
            { Common.config_breadcrumbs =
                [ Common.Breadcrumb
                    { Common.breadcrumb_name = "Home"
                    , Common.breadcrumb_route = Nothing
                    }
                ]
            , Common.config_user = fmap User.githubLogin maybeUser
            }
        , Common.monadoc_page = Index
            { index_packages = fmap
                (\ package -> Package
                    { package_name = Package.name package
                    , package_route = Route.Package $ Package.name package
                    })
                packages
            }
        }

newtype Index = Index
    { index_packages :: [Package]
    } deriving (Eq, Show)

instance ToXml.ToXml Index where
    toXml index = Xml.node "index" []
        [ Xml.node "packages" [] . fmap ToXml.toXml $ index_packages index
        ]

data Package = Package
    { package_name :: PackageName.PackageName
    , package_route :: Route.Route
    } deriving (Eq, Show)

instance ToXml.ToXml Package where
    toXml package = Xml.node "package" []
        [ Xml.node "name" [] [ToXml.toXml $ package_name package]
        , Xml.node "route" [] [ToXml.toXml $ package_route package]
        ]
