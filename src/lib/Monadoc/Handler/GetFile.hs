{-# LANGUAGE TypeApplications #-}

module Monadoc.Handler.GetFile where

import Monadoc.Prelude

import qualified Control.Monad.Catch as Exception
import qualified Data.ByteString as ByteString
import qualified Monadoc.Exception.MissingBlob as MissingBlob
import qualified Monadoc.Exception.NotFound as NotFound
import qualified Monadoc.Model.Blob as Blob
import qualified Monadoc.Model.Distribution as Distribution
import qualified Monadoc.Model.File as File
import qualified Monadoc.Server.Response as Response
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Handler as Handler
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Type.PackageName as PackageName
import qualified Monadoc.Type.Release as Release
import qualified Network.HTTP.Types as Http
import qualified Network.HTTP.Types.Header as Http
import qualified System.FilePath as FilePath
import qualified Witch

handler
    :: PackageName.PackageName
    -> Release.Release
    -> FilePath
    -> Handler.Handler
handler packageName release path context _ = do
    let version = Release.version release
    maybeDistribution <- Context.withConnection context $ \ connection ->
        Distribution.selectByPackageAndVersion connection packageName version
    distribution <- maybe (Exception.throwM NotFound.new) pure maybeDistribution

    maybeFile <- Context.withConnection context $ \ connection ->
        File.selectByDistributionAndPath connection (Model.key distribution) path
    file <- maybe (Exception.throwM NotFound.new) pure maybeFile

    let hash = File.hash $ Model.value file
    maybeBlob <- Context.withConnection context $ \ connection ->
        Blob.selectByHash connection hash
    blob <- maybe (Exception.throwM $ MissingBlob.new hash) pure maybeBlob

    let
        contentType = case FilePath.takeExtensions path of
            ".cabal" -> "text/plain"
            ".hs" -> "text/plain"
            ".md" -> "text/plain"
            _ -> "application/octet-stream"
    pure
        . Response.byteString Http.ok200
            [ (Http.hContentDisposition, Witch.into @ByteString.ByteString $ "filename=" <> path)
            , (Http.hContentType, Witch.into @ByteString.ByteString contentType)
            ]
        . Blob.contents
        $ Model.value blob
