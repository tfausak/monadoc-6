module Monadoc.Worker.Main where

import qualified Control.Concurrent as Concurrent
import qualified Control.Monad as Monad
import qualified Monadoc.Type.Context as Context

run :: Context.Context -> IO ()
run _ = Monad.forever $ do
    Concurrent.threadDelay 1000000
