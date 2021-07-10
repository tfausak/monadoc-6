module Monadoc.Handler.GetFavicon where

import qualified Control.Monad.Catch as Exception
import qualified Monadoc.Exception.Found as Found
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Handler as Handler
import qualified Monadoc.Type.Route as Route

handler :: Handler.Handler
handler context _ = do
    let
        config = Context.config context
        baseUrl = Config.baseUrl config
        location = baseUrl <> Route.toString Route.Logo
    Exception.throwM $ Found.new location
