module Monadoc.Handler.GetModule where

import Monadoc.Prelude

import qualified Monadoc.Class.ToXml as ToXml
import qualified Monadoc.Handler.Common as Common
import qualified Monadoc.Model.User as User
import qualified Monadoc.Type.Breadcrumb as Breadcrumb
import qualified Monadoc.Type.ComponentId as ComponentId
import qualified Monadoc.Type.Handler as Handler
import qualified Monadoc.Type.Meta as Meta
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Type.ModuleName as ModuleName
import qualified Monadoc.Type.PackageName as PackageName
import qualified Monadoc.Type.Revision as Revision
import qualified Monadoc.Type.Root as Root
import qualified Monadoc.Type.Route as Route
import qualified Monadoc.Type.Version as Version
import qualified Monadoc.Utility.Xml as Xml

handler
    :: PackageName.PackageName
    -> Version.Version
    -> Revision.Revision
    -> ComponentId.ComponentId
    -> ModuleName.ModuleName
    -> Handler.Handler
handler packageName version revision componentId moduleName context request = do
    maybeUser <- Common.getUser context request
    let route = Route.Module packageName version revision componentId moduleName
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
                    { Breadcrumb.name = into @String version <> if revision == Revision.zero then "" else "-" <> into @String revision
                    , Breadcrumb.route = Just $ Route.Revision packageName version revision
                    }
                , Breadcrumb.Breadcrumb
                    { Breadcrumb.name = into @String componentId
                    , Breadcrumb.route = Just $ Route.Component packageName version revision componentId
                    }
                , Breadcrumb.Breadcrumb
                    { Breadcrumb.name = into @String moduleName
                    , Breadcrumb.route = Nothing
                    }
                ]
            , Meta.user = fmap (User.githubLogin . Model.value) maybeUser
            }
        , Root.page = Xml.node "module" []
            [ Xml.node "package" [] [ToXml.toXml packageName]
            , Xml.node "version" [] [ToXml.toXml version]
            , Xml.node "revision" [] [ToXml.toXml revision]
            , Xml.node "component" [] [ToXml.toXml $ into @String componentId]
            , Xml.node "module" [] [ToXml.toXml moduleName]
            ]
        }
