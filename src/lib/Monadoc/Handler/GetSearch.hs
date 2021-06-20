module Monadoc.Handler.GetSearch where

import Monadoc.Prelude

import qualified Data.Containers.ListUtils as Containers
import qualified Data.Maybe as Maybe
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

handler :: Maybe String -> Handler.Handler
handler maybeQuery context request = do
    let query = Maybe.fromMaybe "" maybeQuery
    exactMatches <- Context.withConnection context $ \ connection ->
        Package.selectNamesLike connection $ Package.escapeLike query
    partialMatches <- Context.withConnection context $ \ connection ->
        Package.selectNamesLike connection $ "%" <> Package.escapeLike query <> "%"
    maybeUser <- Common.getUser context request
    let
        route = Route.Search maybeQuery
        breadcrumbs =
            [ Breadcrumb.Breadcrumb
                { Breadcrumb.name = "Home"
                , Breadcrumb.route = Just Route.Index
                }
            , Breadcrumb.Breadcrumb
                { Breadcrumb.name = "Search"
                , Breadcrumb.route = Nothing
                }
            ]
        page = Xml.node "search" []
            [ Xml.node "query" [] [ToXml.toXml maybeQuery]
            , Xml.node "packages" []
            . fmap (\ x -> Xml.node "package" []
                [ Xml.node "name" [] [ToXml.toXml x]
                , Xml.node "route" [] [ToXml.toXml $ Route.Package x]
                ])
            . Containers.nubOrd
            $ exactMatches <> partialMatches
            ]
    pure $ Common.makeResponse Root.Root
        { Root.meta = (Meta.fromContext context route)
            { Meta.breadcrumbs
            , Meta.user = fmap (User.githubLogin . Model.value) maybeUser
            }
        , Root.page
        }
