module Monadoc.Utility.Client where

import Monadoc.Prelude

import qualified GHC.Clock as Clock
import qualified Monadoc.Server.Settings as Settings
import qualified Monadoc.Type.RequestId as RequestId
import qualified Monadoc.Utility.Log as Log
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Types as Http
import qualified Network.URI as Uri
import qualified Text.Printf as Printf

performRequest :: Client.Manager -> Client.Request -> IO (Client.Response LazyByteString)
performRequest manager request = do
    let
        oldHeaders = Client.requestHeaders request
        newHeaders = (Http.hUserAgent, userAgent) : oldHeaders
    requestId <- RequestId.random
    before <- Clock.getMonotonicTime
    response <- Client.httpLbs request { Client.requestHeaders = newHeaders } manager
    after <- Clock.getMonotonicTime
    Log.info $ Printf.printf "[http-client/%04x] %s %s %d %.3f"
        (into @Word16 requestId)
        (unsafeInto @Text $ Client.method request)
        (Uri.uriToString identity (Client.getUri request) "")
        (Http.statusCode $ Client.responseStatus response)
        (after - before)
    pure response

userAgent :: ByteString
userAgent = Settings.serverName
