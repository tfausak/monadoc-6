module Monadoc.Handler.GetIndex where

import Monadoc.Prelude

import qualified Data.Time as Time
import qualified Monadoc.Class.ToXml as ToXml
import qualified Monadoc.Handler.Common as Common
import qualified Monadoc.Model.Package as Package
import qualified Monadoc.Model.User as User
import qualified Monadoc.Type.Breadcrumb as Breadcrumb
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Handler as Handler
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Type.PackageName as PackageName
import qualified Monadoc.Type.Revision as Revision
import qualified Monadoc.Type.Route as Route
import qualified Monadoc.Type.Version as Version
import qualified Monadoc.Utility.Xml as Xml

handler :: Handler.Handler
handler context request = do
    let route = Route.Index
    maybeUser <- Common.getUser context request
    packages <- Context.withConnection context Package.selectRecent
    pure $ Common.makeResponse Common.Monadoc
        { Common.monadoc_config = (Common.config_fromContext context route)
            { Common.config_breadcrumbs =
                [ Breadcrumb.Breadcrumb
                    { Breadcrumb.name = "Home"
                    , Breadcrumb.route = Nothing
                    }
                ]
            , Common.config_user = fmap (User.githubLogin . Model.value) maybeUser
            }
        , Common.monadoc_page = Index
            { index_packages = fmap
                (\ package -> Package
                    { package_name = Package.name $ Model.value package
                    , package_version = Package.version $ Model.value package
                    , package_revision = Package.revision $ Model.value package
                    , package_route = Route.Revision
                        (Package.name $ Model.value package)
                        (Package.version $ Model.value package)
                        (Package.revision $ Model.value package)
                    , package_uploadedAt = Package.uploadedAt $ Model.value package
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
    , package_version :: Version.Version
    , package_revision :: Revision.Revision
    , package_route :: Route.Route
    , package_uploadedAt :: Time.UTCTime
    } deriving (Eq, Show)

instance ToXml.ToXml Package where
    toXml package = Xml.node "package" []
        [ Xml.node "name" [] [ToXml.toXml $ package_name package]
        , Xml.node "version" [] [ToXml.toXml $ package_version package]
        , Xml.node "revision" [] [ToXml.toXml $ package_revision package]
        , Xml.node "route" [] [ToXml.toXml $ package_route package]
        , Xml.node "uploadedAt" [] [ToXml.toXml $ package_uploadedAt package]
        ]
