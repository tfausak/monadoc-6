module Monadoc.Handler.GetVersion where

import Monadoc.Prelude

import qualified Data.Ord as Ord
import qualified Data.Pool as Pool
import qualified Monadoc.Exception.NotFound as NotFound
import qualified Monadoc.Model.Package as Package
import qualified Monadoc.Server.Response as Response
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Handler as Handler
import qualified Monadoc.Type.PackageName as PackageName
import qualified Monadoc.Type.Route as Route
import qualified Monadoc.Type.Version as Version
import qualified Network.HTTP.Types as Http

handler :: PackageName.PackageName -> Version.Version -> Handler.Handler
handler packageName version context _ = do
    packages <- Pool.withResource (Context.pool context) $ \ connection ->
        Package.selectByNameAndVersion connection packageName version
    revision <- case maximum $ fmap Package.revision packages of
        Nothing -> throwM NotFound.new
        Just revision -> pure revision
    let
        config = Context.config context
        baseUrl = Config.baseUrl config
        route = Route.Revision packageName version revision
        location = into @ByteString $ baseUrl <> Route.toString route
    pure $ Response.status Http.found302 [(Http.hLocation, location)]

maximum :: (Foldable t, Ord a) => t a -> Maybe a
maximum = maximumOn identity

maximumOn :: (Foldable t, Ord b) => (a -> b) -> t a -> Maybe a
maximumOn = maximumBy . Ord.comparing

maximumBy :: Foldable t => (a -> a -> Ordering) -> t a -> Maybe a
maximumBy f =
    let g x = Just . maybe x (maxBy f x)
    in foldr g Nothing

maxBy :: (a -> a -> Ordering) -> a -> a -> a
maxBy f x y = case f x y of
    GT -> x
    _ -> y
