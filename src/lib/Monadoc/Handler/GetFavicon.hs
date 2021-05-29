module Monadoc.Handler.GetFavicon (handler) where

import Monadoc.Prelude

import qualified Data.ByteString as ByteString
import qualified Monadoc.Server.Response as Response
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Handler as Handler
import qualified Monadoc.Type.Route as Route
import qualified Network.HTTP.Types as Http
import qualified Witch

handler :: Handler.Handler
handler context _ = do
    let
        status = Http.found302
        config = Context.config context
        baseUrl = Config.baseUrl config
        location = Witch.into @ByteString.ByteString $ baseUrl <> Route.toString Route.Logo
        headers = [(Http.hLocation, location)]
    pure $ Response.status status headers
