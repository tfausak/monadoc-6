module Monadoc.Job.UpdatePreferredVersions where

import Monadoc.Prelude

import qualified Control.Concurrent.STM as Stm
import qualified Data.Map as Map
import qualified Monadoc.Model.PreferredVersions as PreferredVersions
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.PackageName as PackageName
import qualified Monadoc.Type.VersionRange as VersionRange

run
    :: Context.Context
    -> Stm.TVar (Map.Map PackageName.PackageName VersionRange.VersionRange)
    -> IO ()
run context preferredVersionsVar = do
    oldPreferredVersions <- Context.withConnection context PreferredVersions.selectAll
    newPreferredVersions <- Stm.atomically $ Stm.readTVar preferredVersionsVar
    newPreferredVersions
        & Map.toList
        & fmap (uncurry PreferredVersions.new)
        & traverse_ (\ pv -> case Map.lookup (PreferredVersions.packageName pv) oldPreferredVersions of
            Just v | v == PreferredVersions.versionRange pv -> pure ()
            _ -> Context.withConnection context $ \ connection ->
                PreferredVersions.upsert connection pv)
