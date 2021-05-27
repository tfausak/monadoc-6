module Monadoc.Worker.Main where

import qualified Control.Concurrent as Concurrent
import qualified Control.Monad as Monad
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Utility.Log as Log

run :: Context.Context -> IO ()
run context = Monad.forever $ do
    Log.info . show . Config.hackageUrl $ Context.config context
    Concurrent.threadDelay 60000000
