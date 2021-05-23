module Monadoc.Handler.GetFavicon (handler) where

import qualified Monadoc.Server.Response as Response
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Handler as Handler
import qualified Monadoc.Utility.Convert as Convert
import qualified Network.HTTP.Types as Http

handler :: Handler.Handler
handler context _ = do
    let
        status = Http.found302
        config = Context.config context
        baseUrl = Config.baseUrl config
        location = Convert.stringToUtf8 $ baseUrl <> "/monadoc.svg"
        headers = [(Http.hLocation, location)]
    pure $ Response.status status headers
