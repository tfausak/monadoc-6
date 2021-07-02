module Monadoc.Handler.GetFile where

import Monadoc.Prelude

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
import qualified Monadoc.Type.Version as Version
import qualified Network.HTTP.Types as Http
import qualified Network.HTTP.Types.Header as Http

handler
    :: PackageName.PackageName
    -> Version.Version
    -> FilePath
    -> Handler.Handler
handler packageName version path context _ = do
    maybeDistribution <- Context.withConnection context $ \ connection ->
        Distribution.selectByPackageAndVersion connection packageName version
    distribution <- maybe (throwM NotFound.new) pure maybeDistribution

    maybeFile <- Context.withConnection context $ \ connection ->
        File.selectByDistributionAndPath connection (Model.key distribution) path
    file <- maybe (throwM NotFound.new) pure maybeFile

    let hash = File.hash $ Model.value file
    maybeBlob <- Context.withConnection context $ \ connection ->
        Blob.selectByHash connection hash
    blob <- maybe (throwM $ MissingBlob.new hash) pure maybeBlob

    pure
        . Response.byteString Http.ok200
            [ (Http.hContentDisposition, into @ByteString $ "filename=" <> File.path (Model.value file))
            , (Http.hContentType, into @ByteString "application/octet-stream")
            ]
        . Blob.contents
        $ Model.value blob