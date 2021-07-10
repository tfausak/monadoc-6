{-# LANGUAGE TypeApplications #-}

module Monadoc.Job.UnpackDistribution where

import Monadoc.Prelude

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Compression.GZip as Gzip
import qualified Control.Concurrent.STM as Stm
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Maybe as Maybe
import qualified Data.Time as Time
import qualified Monadoc.Exception.MissingBlob as MissingBlob
import qualified Monadoc.Exception.UnexpectedTarEntry as UnexpectedTarEntry
import qualified Monadoc.Model.Blob as Blob
import qualified Monadoc.Model.Distribution as Distribution
import qualified Monadoc.Model.File as File
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Model as Model
import qualified System.FilePath as FilePath
import qualified System.FilePath.Posix as FilePath.Posix

run :: Context.Context -> Distribution.Model -> IO ()
run context distribution = do
    let hash = Distribution.hash $ Model.value distribution
    maybeBlob <- Context.withConnection context $ \ connection ->
        Blob.selectByHash connection hash
    blob <- case maybeBlob of
        Nothing -> throwM $ MissingBlob.new hash
        Just blob -> pure blob
    pathVar <- Stm.newEmptyTMVarIO
    blob
        & Model.value
        & Blob.contents
        & into @LazyByteString.ByteString
        & Gzip.decompress
        & Tar.read
        & Tar.foldEntries ((:) . Right) [] (pure . Left)
        & traverse_ (unpackDistributionItem context (Model.key distribution) pathVar)
    now <- Time.getCurrentTime
    Context.withConnection context $ \ connection ->
        Distribution.updateUnpackedAt connection (Model.key distribution) (Just now)

unpackDistributionItem
    :: Context.Context
    -> Distribution.Key
    -> Stm.TMVar FilePath
    -> Either Tar.FormatError Tar.Entry
    -> IO ()
unpackDistributionItem context distribution pathVar item = case item of
    Left formatError -> case formatError of
        Tar.ShortTrailer ->
            -- Hackage itself ignores this error in many places. See
            -- <https://github.com/haskell/hackage-server/issues/851>.
            pure ()
        _ ->
            throwM formatError
    Right entry -> case Tar.entryContent entry of
        Tar.Directory ->
            -- We only care about files, so we can safely ignore directories.
            pure ()
        Tar.OtherEntryType '5' _ _ ->
            -- DIRTYPE: These are also directories.
            pure ()
        Tar.OtherEntryType 'g' _ _ ->
            -- XGLTYPE: These @pax\_global\_header@ files can be ignored. See
            -- <https://github.com/haskell/hackage-server/pull/190>.
            pure ()
        Tar.OtherEntryType 'x' _ _ ->
            -- XHDTYPE: These extended attributes can be ignored. See
            -- <https://github.com/haskell/hackage-server/issues/858>.
            pure ()
        Tar.SymbolicLink _ ->
            -- No symbolic links appear to be necessary for our purposes. See
            -- <https://github.com/haskell/hackage-server/issues/858>.
            pure ()
        Tar.HardLink _ ->
            -- Similarly, hard links appear to be unnecessary. See
            -- <https://github.com/haskell/hackage-server/issues/858>.
            pure ()
        Tar.OtherEntryType 'L' contents _ -> do
            path <- either throwM pure $ tryInto @String contents
            succeeded <- Stm.atomically $ Stm.tryPutTMVar pathVar path
            unless succeeded . throwM $ UnexpectedTarEntry.new entry
        Tar.NormalFile contents _ -> do
            maybePath <- Stm.atomically $ Stm.tryTakeTMVar pathVar
            let
                path = normalizeFilePath $ Maybe.fromMaybe (Tar.entryPath entry) maybePath
                blob = Blob.fromByteString $ into @ByteString contents
                file = File.File { File.distribution = distribution, File.hash = Blob.hash blob, File.path = path }
            Context.withConnection context $ \ connection -> do
                Blob.upsert connection blob
                File.upsert connection file
        _ -> throwM $ UnexpectedTarEntry.new entry

-- Note that on Windows, tar entries have Windows-style paths:
--
-- - Windows: base\4.15.0.0\base.cabal
-- - Unix: base/4.15.0.0/base.cabal
normalizeFilePath :: FilePath -> FilePath
normalizeFilePath = FilePath.Posix.joinPath . FilePath.splitDirectories
