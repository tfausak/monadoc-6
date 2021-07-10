{-# LANGUAGE TypeApplications #-}

module Monadoc.Utility.Client where

import Monadoc.Prelude

import qualified Control.Monad.Catch as Exception
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Text as Text
import qualified Data.Word as Word
import qualified GHC.Clock as Clock
import qualified Monadoc.Server.Settings as Settings
import qualified Monadoc.Type.RequestId as RequestId
import qualified Monadoc.Utility.Log as Log
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Types as Http
import qualified Network.URI as Uri
import qualified Text.Printf as Printf
import qualified Witch

performRequest :: Client.Manager -> Client.Request -> IO (Client.Response LazyByteString.ByteString)
performRequest manager request = do
    let
        oldHeaders = Client.requestHeaders request
        newHeaders = (Http.hUserAgent, userAgent) : oldHeaders
    requestId <- RequestId.random
    before <- Clock.getMonotonicTime
    response <- Client.httpLbs request { Client.requestHeaders = newHeaders } manager
    after <- Clock.getMonotonicTime
    method <- either Exception.throwM pure . Witch.tryInto @Text.Text $ Client.method request
    Log.info $ Printf.printf "[client/%04x] %s %s %d %.3f"
        (Witch.into @Word.Word16 requestId)
        method
        (Uri.uriToString id (Client.getUri request) "")
        (Http.statusCode $ Client.responseStatus response)
        (after - before)
    pure response

userAgent :: ByteString.ByteString
userAgent = Settings.serverName
