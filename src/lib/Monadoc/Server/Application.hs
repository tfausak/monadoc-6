module Monadoc.Server.Application where

import qualified Data.ByteString.Lazy as LazyByteString
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai

application :: Wai.Application
application _ respond =
    respond $ Wai.responseLBS Http.ok200 [] LazyByteString.empty
