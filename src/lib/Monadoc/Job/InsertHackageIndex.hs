module Monadoc.Job.InsertHackageIndex where

import Monadoc.Prelude

import qualified Codec.Compression.GZip as Gzip
import qualified Data.ByteString as ByteString
import qualified Monadoc.Model.HackageIndex as HackageIndex
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Utility.Log as Log
import qualified Monadoc.Vendor.Client as Client

run :: Context.Context -> IO HackageIndex.HackageIndex
run context = do
    Log.info "[worker] getting initial hackage index"
    request <- Client.parseUrlThrow $ Config.hackageUrl (Context.config context) <> "/01-index.tar.gz"
    response <- Client.performRequest (Context.manager context) request
    let
        contents = Client.responseBody response
            & Gzip.decompress
            & into @ByteString
        size = ByteString.length contents
        hackageIndex = HackageIndex.HackageIndex { HackageIndex.contents, HackageIndex.size }
    Log.info $ "[worker] got initial hackage index (" <> show size <> ")"
    Context.withConnection context $ \ connection ->
        HackageIndex.insert connection hackageIndex
    pure hackageIndex