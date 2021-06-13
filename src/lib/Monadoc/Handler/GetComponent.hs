module Monadoc.Handler.GetComponent where

import Monadoc.Prelude

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Pool as Pool
import qualified Monadoc.Exception.NotFound as NotFound
import qualified Monadoc.Handler.Common as Common
import qualified Monadoc.Model.Component as Component
import qualified Monadoc.Model.Package as Package
import qualified Monadoc.Model.User as User
import qualified Monadoc.Type.ComponentId as ComponentId
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Handler as Handler
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Type.PackageName as PackageName
import qualified Monadoc.Type.Revision as Revision
import qualified Monadoc.Type.Route as Route
import qualified Monadoc.Type.Version as Version
import qualified Monadoc.Utility.Log as Log

handler
    :: PackageName.PackageName
    -> Version.Version
    -> Revision.Revision
    -> ComponentId.ComponentId
    -> Handler.Handler
handler packageName version revision componentId context request = do
    let route = Route.Component packageName version revision componentId
    maybeUser <- Common.getUser context request
    maybePackage <- Pool.withResource (Context.pool context) $ \ connection ->
        Package.select connection packageName version revision
    package <- maybe (throwM NotFound.new) pure maybePackage
    allComponents <- Pool.withResource (Context.pool context) $ \ connection ->
        Component.selectByPackage connection $ Model.key package
    let componentsByTag = groupBy (Component.tag . Model.value) allComponents
    components <- case Map.lookup (ComponentId.tag componentId) componentsByTag of
        Nothing -> throwM NotFound.new
        Just components -> pure components
    Log.info $ show components -- TODO
    pure $ Common.makeResponse Common.Monadoc
        { Common.monadoc_config = (Common.config_fromContext context route)
            { Common.config_breadcrumbs =
                [ Common.Breadcrumb
                    { Common.breadcrumb_name = "Home"
                    , Common.breadcrumb_route = Just Route.Index
                    }
                , Common.Breadcrumb
                    { Common.breadcrumb_name = into @String packageName
                    , Common.breadcrumb_route = Just $ Route.Package packageName
                    }
                , Common.Breadcrumb
                    { Common.breadcrumb_name = into @String version
                    , Common.breadcrumb_route = Just $ Route.Version packageName version
                    }
                , Common.Breadcrumb
                    { Common.breadcrumb_name = into @String revision
                    , Common.breadcrumb_route = Just $ Route.Revision packageName version revision
                    }
                , Common.Breadcrumb
                    { Common.breadcrumb_name = into @String componentId
                    , Common.breadcrumb_route = Nothing
                    }
                ]
            , Common.config_user = fmap (User.githubLogin . Model.value) maybeUser
            }
        , Common.monadoc_page = "TODO"
        }

groupBy :: (Ord k, Foldable t) => (v -> k) -> t v -> Map k (NonEmpty v)
groupBy f = foldr
    (\ v -> Map.alter
        (Just . maybe (NonEmpty.singleton v) (NonEmpty.cons v))
        (f v))
    Map.empty
