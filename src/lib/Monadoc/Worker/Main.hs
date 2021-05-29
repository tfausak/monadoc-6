module Monadoc.Worker.Main where

import Monadoc.Prelude

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Compression.GZip as Gzip
import qualified Control.Concurrent as Concurrent
import qualified Control.Monad as Monad
import qualified Data.ByteString as ByteString
import qualified Data.Maybe as Maybe
import qualified Data.Pool as Pool
import qualified Monadoc.Model.HackageIndex as HackageIndex
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Utility.Log as Log
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Types as Http
import qualified System.IO.Unsafe as Unsafe
import qualified Text.Read as Read

run :: Context.Context -> IO ()
run context = Monad.forever <| do
    Log.info "starting"
    let
        hackageUrl = Config.hackageUrl <| Context.config context
        manager = Context.manager context
    maybeOldHackageIndex <- Pool.withResource (Context.pool context) HackageIndex.select
    case maybeOldHackageIndex of
        Nothing -> do
            Log.info "hackage index does not exist yet"
            request <- Client.parseUrlThrow <| hackageUrl <> "/01-index.tar.gz"
            response <- Client.httpLbs request manager
            let
                contents = Client.responseBody response
                    |> Gzip.decompress
                    |> into @ByteString
                size = ByteString.length contents
                newHackageIndex = HackageIndex.HackageIndex { HackageIndex.contents, HackageIndex.size }
            Log.info <| "got hackage index: " <> show size
            Pool.withResource (Context.pool context) <| \ connection -> do
                HackageIndex.insert connection newHackageIndex
        Just oldHackageIndex -> do
            let oldSize = HackageIndex.size oldHackageIndex
            Log.info <| "hackage index already exists: " <> show oldSize
            request <- Client.parseUrlThrow <| hackageUrl <> "/01-index.tar"
            headResponse <- Client.httpNoBody
                request { Client.method = into @ByteString "HEAD" }
                manager
            let
                newSize = Maybe.fromMaybe 0 <| do
                    x <- lookup Http.hContentLength <| Client.responseHeaders headResponse
                    y <- hush <| tryInto @String x
                    Read.readMaybe @Int y
            Log.info <| "hackage index old size: " <> show oldSize <> ", new size: " <> show newSize
            if newSize == oldSize
                then Log.info "nothing to do"
                else do
                    let
                        index = oldSize - HackageIndex.offset
                        range = "bytes=" <> show index <> "-" <> show (newSize - 1)
                    Log.info <| "updating hackage index: " <> show range
                    rangeResponse <- Client.httpLbs
                        request { Client.requestHeaders = (Http.hRange, into @ByteString range) : Client.requestHeaders request }
                        manager
                    let
                        before = ByteString.take index <| HackageIndex.contents oldHackageIndex
                        after = into @ByteString <| Client.responseBody rangeResponse
                        contents = before <> after
                        newHackageIndex = HackageIndex.fromByteString contents
                    Pool.withResource (Context.pool context) <| \ connection -> do
                        HackageIndex.update connection newHackageIndex
    Log.info "counting entries in the hackage index"
    maybeHackageIndex <- Pool.withResource (Context.pool context) HackageIndex.select
    case maybeHackageIndex of
        Nothing -> pure ()
        Just hackageIndex -> do
            hackageIndex
                |> HackageIndex.contents
                |> into @LazyByteString
                |> Tar.read
                |> Tar.foldEntries (:) [] (Unsafe.unsafePerformIO <. throwM)
                |> length
                |> show
                |> Log.info
            pure ()
    Log.info "done. waiting a minute"
    Concurrent.threadDelay 60000000
