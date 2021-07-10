{-# LANGUAGE TypeApplications #-}

module Monadoc.Job.UpdateHackageIndex where

import Monadoc.Prelude

import qualified Data.ByteString as ByteString
import qualified Monadoc.Exception.BadHackageIndexSize as BadHackageIndexSize
import qualified Monadoc.Model.HackageIndex as HackageIndex
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Utility.Either as Either
import qualified Monadoc.Utility.Log as Log
import qualified Monadoc.Vendor.Client as Client
import qualified Network.HTTP.Types as Http
import qualified Text.Read as Read

run :: Context.Context -> Model.Model HackageIndex.HackageIndex -> IO HackageIndex.HackageIndex
run context model = do
    let
        oldHackageIndex = Model.value model
        oldSize = HackageIndex.size oldHackageIndex
    Log.info $ "[worker] checking for new hackage index (" <> show oldSize <> ")"
    request <- Client.parseUrlThrow $ Config.hackageUrl (Context.config context) <> "/01-index.tar"
    headResponse <- Client.performRequest (Context.manager context)
        request { Client.method = Http.methodHead }
    let
        maybeNewSize = do
            x <- lookup Http.hContentLength $ Client.responseHeaders headResponse
            y <- Either.toMaybe $ tryInto @String x
            Read.readMaybe @Int y
    case maybeNewSize of
        Nothing -> throwM $ BadHackageIndexSize.new oldSize maybeNewSize
        Just newSize
            | newSize < oldSize -> throwM $ BadHackageIndexSize.new oldSize maybeNewSize
            | newSize == oldSize -> do
                Log.info "[worker] hackage index has not changed"
                pure oldHackageIndex
            | otherwise -> do
                let
                    delta = newSize - oldSize
                    start = oldSize - HackageIndex.offset
                    end = newSize - 1
                    range = into @ByteString $ "bytes=" <> show start <> "-" <> show end
                Log.info $ "[worker] getting new hackage index (" <> show delta <> ")"
                rangeResponse <- Client.performRequest (Context.manager context)
                    request { Client.requestHeaders = (Http.hRange, range) : Client.requestHeaders request }
                Log.info "[worker] got new hackage index"
                let
                    before = ByteString.take start $ HackageIndex.contents oldHackageIndex
                    after = into @ByteString $ Client.responseBody rangeResponse
                    contents = before <> after
                    newHackageIndex = HackageIndex.fromByteString contents
                    key = Model.key model
                Context.withConnection context $ \ connection ->
                    HackageIndex.update connection key newHackageIndex
                pure newHackageIndex
