module Monadoc.Worker.Main where

import Monadoc.Prelude

import qualified Control.Concurrent as Concurrent
import qualified Control.Monad as Monad
import qualified Monadoc.Job.FetchDistributions as FetchDistributions
import qualified Monadoc.Job.ProcessHackageIndex as ProcessHackageIndex
import qualified Monadoc.Job.UnpackDistributions as UnpackDistributions
import qualified Monadoc.Job.UpsertHackageIndex as UpsertHackageIndex
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Utility.Log as Log

run :: Context.Context -> IO ()
run context = do
    Log.info "starting worker"
    Monad.forever $ do
        Log.info "beginning worker loop"
        hackageIndex <- UpsertHackageIndex.run context
        ProcessHackageIndex.run context hackageIndex
        FetchDistributions.run context
        UnpackDistributions.run context
        -- TODO: Process distributions (in other words, parse Haskell modules).
        Log.info "finished worker loop"
        Concurrent.threadDelay 60000000
