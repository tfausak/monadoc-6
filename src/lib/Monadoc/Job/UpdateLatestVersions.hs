module Monadoc.Job.UpdateLatestVersions where

import Monadoc.Prelude

import qualified Control.Concurrent.STM as Stm
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Monadoc.Model.LatestVersion as LatestVersion
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.PackageName as PackageName
import qualified Monadoc.Type.Revision as Revision
import qualified Monadoc.Type.Version as Version
import qualified Monadoc.Type.VersionRange as VersionRange
import qualified Monadoc.Utility.Foldable as Foldable

run
    :: Context.Context
    -> Stm.TVar (Map.Map (PackageName.PackageName, Version.Version) Revision.Revision)
    -> Stm.TVar (Map.Map PackageName.PackageName VersionRange.VersionRange)
    -> IO ()
run context revisionsVar preferredVersionsVar = do
    oldLatestVersions <- Context.withConnection context LatestVersion.selectAll
    revisions <- Stm.atomically $ Stm.readTVar revisionsVar
    preferredVersions <- Stm.atomically $ Stm.readTVar preferredVersionsVar
    let
        newLatestVersions =
            revisions
                & Map.mapMaybe Revision.decrement
                & Map.toList
                & fmap (\ ((p, v), r) ->
                    let c = Map.findWithDefault VersionRange.any p preferredVersions
                    in (p, [(VersionRange.contains v c, (v, r))]))
                & Map.fromListWith (<>)
                & Map.mapMaybe Foldable.maximum
                & fmap snd
    newLatestVersions
        & Map.toList
        & mapM_ (\ (p, (v1, r1)) -> case Map.lookup p oldLatestVersions of
            Just (v0, r0) | v0 == v1 && r0 == r1 -> pure ()
            _ -> Context.withConnection context $ \ connection ->
                LatestVersion.upsert connection $ LatestVersion.new p v1 r1)
    Set.difference (Map.keysSet oldLatestVersions) (Map.keysSet newLatestVersions)
        & mapM_ (\ p -> Context.withConnection context $ \ connection ->
            LatestVersion.deleteByPackage connection p)
