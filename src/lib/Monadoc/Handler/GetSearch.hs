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
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Type.PackageName as PackageName
import qualified Monadoc.Type.Route as Route
import qualified Monadoc.Utility.Xml as Xml

handler :: Maybe String -> Handler.Handler
handler maybeQuery context request = do
    let
        route = Route.Search maybeQuery
        query = Maybe.fromMaybe "" maybeQuery
    exactMatches <- Context.withConnection context $ \ connection ->
        Package.selectNamesLike connection $ Package.escapeLike query
    partialMatches <- Context.withConnection context $ \ connection ->
        Package.selectNamesLike connection $ "%" <> Package.escapeLike query <> "%"
    maybeUser <- Common.getUser context request
    pure $ Common.makeResponse Common.Root
        { Common.root_config = (Common.config_fromContext context route)
            { Common.config_breadcrumbs =
                [ Breadcrumb.Breadcrumb
                    { Breadcrumb.name = "Home"
                    , Breadcrumb.route = Just Route.Index
                    }
                , Breadcrumb.Breadcrumb
                    { Breadcrumb.name = "Search"
                    , Breadcrumb.route = Nothing
                    }
                ]
            , Common.config_user = fmap (User.githubLogin . Model.value) maybeUser
            }
        , Common.root_page = Search
            { search_query = query
            ,search_packages = fmap (\ packageName -> Package
                { package_name = packageName
                , package_route = Route.Package packageName
                }) . Containers.nubOrd $ exactMatches <> partialMatches
            }
        }

data Search = Search
    { search_query :: String
    , search_packages :: [Package]
    } deriving (Eq, Show)

instance ToXml.ToXml Search where
    toXml search = Xml.node "search" []
        [ Xml.node "query" [] [ToXml.toXml $ search_query search]
        , Xml.node "packages" [] . fmap ToXml.toXml $ search_packages search
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
