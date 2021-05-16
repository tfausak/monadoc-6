module Monadoc.Server.Middleware where

import qualified Control.Monad.Catch as Exception
import qualified Data.ByteString as ByteString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text
import qualified GHC.Clock as Clock
import qualified Monadoc.Log as Log
import qualified Monadoc.Server.Settings as Settings
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
            (fromUtf8 $ Wai.requestMethod request)
            (fromUtf8 $ Wai.rawPathInfo request)
            (fromUtf8 $ Wai.rawQueryString request)
            (Http.statusCode $ Wai.responseStatus response)
            (after - before)
        respond response

handleExceptions :: Wai.Middleware
handleExceptions handle request respond =
    Exception.catch (handle request respond) $ \ exception -> do
        Settings.onException (Just request) exception
        respond $ Settings.onExceptionResponse exception

fromUtf8 :: ByteString.ByteString -> Text.Text
fromUtf8 = Text.decodeUtf8With Text.lenientDecode
