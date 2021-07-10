module Monadoc.Worker.Main where

import Monadoc.Prelude

import qualified Control.Concurrent as Concurrent
import qualified Control.Monad as Monad
import qualified Monadoc.Job.FetchDistributions as FetchDistributions
import qualified Monadoc.Job.ProcessDistributions as ProcessDistributions
import qualified Monadoc.Job.ProcessHackageIndex as ProcessHackageIndex
import qualified Monadoc.Job.UnpackDistributions as UnpackDistributions
import qualified Monadoc.Job.UpsertHackageIndex as UpsertHackageIndex
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Utility.Log as Log

run :: Context.Context -> IO ()
run context = do
    Log.info "[worker] initializing"
    Monad.forever $ do
        Log.info "[worker] starting loop"
        hackageIndex <- UpsertHackageIndex.run context
        ProcessHackageIndex.run context hackageIndex
        FetchDistributions.run context
        UnpackDistributions.run context
        Monad.when False $ ProcessDistributions.run context -- TODO
        Log.info "[worker] finished loop"
        Concurrent.threadDelay 60000000
