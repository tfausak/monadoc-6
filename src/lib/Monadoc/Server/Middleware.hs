module Monadoc.Server.Middleware where

import Monadoc.Prelude

import qualified Data.CaseInsensitive as CI
import qualified GHC.Clock as Clock
import qualified Monadoc.Server.Settings as Settings
import qualified Monadoc.Type.RequestId as RequestId
import qualified Monadoc.Utility.Log as Log
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Text.Printf as Printf

-- If the middleware is defined as @middleware = a . b@, then @a@ will be
-- "outside" and @b@ will be "inside". In other words @a@ will get the request
-- first, then it will go to @b@, then the application will handle it, then @b@
-- will get the response, then it will go to @a@.
middleware :: Wai.Middleware
middleware = addRequestId . logRequests . addSecurityHeaders . handleExceptions

addRequestId :: Wai.Middleware
addRequestId f request respond = do
    requestId <- RequestId.random
    f (RequestId.set requestId request) respond

logRequests :: Wai.Middleware
logRequests f request respond = do
    before <- Clock.getMonotonicTime
    f request $ \ response -> do
        after <- Clock.getMonotonicTime
        method <- either throwM pure . tryInto @Text $ Wai.requestMethod request
        path <- either throwM pure . tryInto @Text $ Wai.rawPathInfo request
        query <- either throwM pure . tryInto @Text $ Wai.rawQueryString request
        Log.info $ Printf.printf "[server/%04x] %s %s%s %d %.3f"
            (maybe 0 (into @Word16) $ RequestId.get request)
            method
            path
            query
            (Http.statusCode $ Wai.responseStatus response)
            (after - before)
        respond response

addSecurityHeaders :: Wai.Middleware
addSecurityHeaders =
    let
        (=:) :: String -> String -> Http.Header
        k =: v = (CI.mk $ into @ByteString k, into @ByteString v)
    in Wai.modifyResponse . Wai.mapResponseHeaders $ \ headers ->
        "Content-Security-Policy" =: "default-src 'self'"
        : "Referrer-Policy" =: "same-origin"
        : "Strict-Transport-Security" =: "max-age=60; includeSubDomains"
        : "X-Content-Type-Options" =: "nosniff"
        : "X-Frame-Options" =: "SAMEORIGIN"
        : "X-Xss-Protection" =: "1; mode=block"
        : headers

handleExceptions :: Wai.Middleware
handleExceptions f request respond =
    catch (f request respond) $ \ exception -> do
        Settings.onException (Just request) exception
        respond $ Settings.onExceptionResponse exception
