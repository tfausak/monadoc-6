module Monadoc.Job.UnpackDistributions (run) where

import qualified Monadoc.Job.UnpackDistribution as UnpackDistribution
import qualified Monadoc.Model.Distribution as Distribution
import qualified Monadoc.Type.Context as Context

run :: Context.Context -> IO ()
run context = do
    distributions <- Context.withConnection context Distribution.selectUnpacked
    mapM_ (UnpackDistribution.run context) distributions
