module Monadoc.Handler.GetSearch where

import Monadoc.Prelude

import qualified Data.Containers.ListUtils as Containers
import qualified Data.Pool as Pool
import qualified Monadoc.Class.ToXml as ToXml
import qualified Monadoc.Handler.Common as Common
import qualified Monadoc.Model.Package as Package
import qualified Monadoc.Model.User as User
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Handler as Handler
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Type.PackageName as PackageName
import qualified Monadoc.Type.Route as Route
import qualified Monadoc.Utility.Xml as Xml
import qualified Network.Wai as Wai

handler :: Handler.Handler
handler context request = do
    let route = Route.Search
    query <- case lookup (into @ByteString "query") $ Wai.queryString request of
        Just (Just q) -> either throwM pure $ tryInto @String q
        _ -> pure $ into @String ""
    exactMatches <- Pool.withResource (Context.pool context) $ \ connection ->
        Package.selectNamesLike connection $ Package.escapeLike query
    partialMatches <- Pool.withResource (Context.pool context) $ \ connection ->
        Package.selectNamesLike connection $ "%" <> Package.escapeLike query <> "%"
    maybeUser <- Common.getUser context request
    pure $ Common.makeResponse Common.Monadoc
        { Common.monadoc_config = (Common.config_fromContext context route)
            { Common.config_breadcrumbs =
                [ Common.Breadcrumb
                    { Common.breadcrumb_name = "Home"
                    , Common.breadcrumb_route = Just Route.Index
                    }
                , Common.Breadcrumb
                    { Common.breadcrumb_name = "Search"
                    , Common.breadcrumb_route = Nothing
                    }
                ]
            , Common.config_user = fmap (User.githubLogin . Model.value) maybeUser
            }
        , Common.monadoc_page = Search
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
