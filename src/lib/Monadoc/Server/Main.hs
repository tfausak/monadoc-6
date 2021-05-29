module Monadoc.Server.Main where

import Monadoc.Prelude

import qualified Monadoc.Server.Application as Application
import qualified Monadoc.Server.Middleware as Middleware
import qualified Monadoc.Server.Settings as Settings
import qualified Monadoc.Type.Context as Context
import qualified Network.Wai.Handler.Warp as Warp

run :: Context.Context -> IO ()
run context =
    Warp.runSettings (Settings.fromConfig $ Context.config context)
        . Middleware.middleware $ Application.application context
