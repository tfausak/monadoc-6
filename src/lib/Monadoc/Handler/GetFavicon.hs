module Monadoc.Handler.GetFavicon where

import Monadoc.Prelude

import qualified Monadoc.Server.Response as Response
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Handler as Handler
import qualified Monadoc.Type.Route as Route
import qualified Network.HTTP.Types as Http

handler :: Handler.Handler
handler context _ = do
    let
        status = Http.found302
        config = Context.config context
        baseUrl = Config.baseUrl config
        location = into @ByteString $ baseUrl <> Route.toString Route.Logo
        headers = [(Http.hLocation, location)]
    pure $ Response.status status headers
