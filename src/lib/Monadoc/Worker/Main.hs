module Monadoc.Worker.Main where

import Monadoc.Prelude

import qualified Control.Concurrent as Concurrent
import qualified Control.Monad as Monad
import qualified Data.CaseInsensitive as CI
import qualified Data.List as List
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Context as Context
import qualified Network.HTTP.Client as Client

run :: Context.Context -> IO ()
run context = Monad.forever $ do
    let
        hackageUrl = Config.hackageUrl $ Context.config context
        manager = Context.manager context
    request <- Client.parseUrlThrow $ hackageUrl <> "/01-index.tar.gz"
    response <- Client.httpNoBody
        request { Client.method = into @ByteString "HEAD" }
        manager
    -- TODO: How do you efficiently update the Hackage index without
    -- re-downloading the whole thing every time?
    putStr
        . unlines
        . fmap (\ (k, v) -> unsafeInto @String $ CI.foldedCase k <> into @ByteString ": " <> v)
        . List.sort
        $ Client.responseHeaders response
    Concurrent.threadDelay 60000000
