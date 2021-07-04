module Monadoc.Job.FetchDistributions where

import Monadoc.Prelude

import qualified Monadoc.Job.FetchDistribution as FetchDistribution
import qualified Monadoc.Model.Distribution as Distribution
import qualified Monadoc.Model.Package as Package
import qualified Monadoc.Type.Context as Context

run :: Context.Context -> IO ()
run context = do
    hashes <- Context.withConnection context Distribution.selectHashes
    namesAndVersions <- Context.withConnection context Package.selectNamesAndVersions
    traverse_ (FetchDistribution.run context hashes) namesAndVersions
