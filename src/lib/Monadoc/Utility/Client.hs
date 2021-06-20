module Monadoc.Utility.Client where

import Monadoc.Prelude

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
        method = unsafeInto @Text $ Client.method request
        uri = Uri.uriToString identity (Client.getUri request) ""
    requestId <- RequestId.random
    Log.info $ Printf.printf "[http-client/%04x] %s %s"
        (into @Word16 requestId) method uri
    response <- Client.httpLbs request { Client.requestHeaders = newHeaders } manager
    Log.info $ Printf.printf "[http-client/%04x] %s %s %d"
        (into @Word16 requestId) method uri (Http.statusCode $ Client.responseStatus response)
    pure response

userAgent :: ByteString
userAgent = Settings.serverName
