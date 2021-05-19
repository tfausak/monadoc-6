module Monadoc.Server.Middleware where

import qualified Control.Monad.Catch as Exception
import qualified GHC.Clock as Clock
import qualified Monadoc.Server.Settings as Settings
import qualified Monadoc.Utility.Convert as Convert
import qualified Monadoc.Utility.Log as Log
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Text.Printf as Printf

-- If the middleware is defined as @middleware = a . b@, then @a@ will be
-- "outside" and @b@ will be "inside". In other words @a@ will get the request
-- first, then it will go to @b@, then the application will handle it, then @b@
-- will get the response, then it will go to @a@.
middleware :: Wai.Middleware
middleware = logRequests . handleExceptions

logRequests :: Wai.Middleware
logRequests handle request respond = do
    before <- Clock.getMonotonicTime
    handle request $ \ response -> do
        after <- Clock.getMonotonicTime
        Log.info $ Printf.printf "%s %s%s %d %.3f"
            (Convert.utf8ToText $ Wai.requestMethod request)
            (Convert.utf8ToText $ Wai.rawPathInfo request)
            (Convert.utf8ToText $ Wai.rawQueryString request)
            (Http.statusCode $ Wai.responseStatus response)
            (after - before)
        respond response

handleExceptions :: Wai.Middleware
handleExceptions handle request respond =
    Exception.catch (handle request respond) $ \ exception -> do
        Settings.onException (Just request) exception
        respond $ Settings.onExceptionResponse exception
