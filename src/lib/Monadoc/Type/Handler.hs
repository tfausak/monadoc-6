module Monadoc.Type.Handler where

import Monadoc.Prelude

import qualified Monadoc.Type.Context as Context
import qualified Network.Wai as Wai

type Handler = Context.Context -> Wai.Request -> IO Wai.Response
