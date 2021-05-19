module Monadoc.Server.Application where

import qualified Monadoc.Server.Response as Response
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai

application :: Wai.Application
application _ respond =
    respond $ Response.status Http.ok200 []
