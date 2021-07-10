module Monadoc.Handler.GetIndex where

import Monadoc.Prelude

import qualified Data.List as List
import qualified Data.Ord as Ord
import qualified Monadoc.Class.ToXml as ToXml
import qualified Monadoc.Handler.Common as Common
import qualified Monadoc.Model.Package as Package
import qualified Monadoc.Model.User as User
import qualified Monadoc.Type.Breadcrumb as Breadcrumb
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Handler as Handler
import qualified Monadoc.Type.Meta as Meta
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Type.Root as Root
import qualified Monadoc.Type.Route as Route
import qualified Monadoc.Utility.Xml as Xml

handler :: Handler.Handler
handler context request = do
    maybeUser <- Common.getUser context request
    packages <- Context.withConnection context Package.selectRecent
    let
        route = Route.Index
        breadcrumbs =
            [ Breadcrumb.Breadcrumb
                { Breadcrumb.name = "Home"
                , Breadcrumb.route = Nothing
                }
            ]
        page = Xml.node "index" []
            [ Xml.node "packages" []
            . fmap (\ x -> Xml.node "package" []
                [ Xml.node "name" [] [ToXml.toXml $ Package.name x]
                , Xml.node "release" [] [ToXml.toXml $ Package.release x]
                , Xml.node "route" []
                    [ ToXml.toXml $ Route.Release (Package.name x) (Package.release x)
                    ]
                , Xml.node "uploadedAt" [] [ToXml.toXml $ Package.uploadedAt x]
                ])
            . List.sortOn (Ord.Down . Package.uploadedAt)
            $ fmap Model.value packages
            ]
    pure $ Common.makeResponse Root.Root
        { Root.meta = (Meta.fromContext context route)
            { Meta.breadcrumbs = breadcrumbs
            , Meta.user = fmap (User.githubLogin . Model.value) maybeUser
            }
        , Root.page = page
        }
