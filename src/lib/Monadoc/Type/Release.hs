{-# LANGUAGE FlexibleInstances #-}

module Monadoc.Type.Release where

import Monadoc.Prelude

import qualified Monadoc.Class.ToXml as ToXml
import qualified Monadoc.Type.Revision as Revision
import qualified Monadoc.Type.Version as Version

data Release = Release
    { version :: Version.Version
    , revision :: Maybe Revision.Revision
    } deriving (Eq, Show)

instance TryFrom String Release where
    tryFrom = maybeTryFrom $ \ string -> do
        let (before, after) = break (== '-') string
        v <- hush $ tryFrom @String before
        case after of
            "" -> pure Release { version = v, revision = Nothing }
            '-' : rest -> do
                r <- hush $ tryFrom @String rest
                pure Release { version = v, revision = Just r }
            _ -> Nothing

instance From Release String where
    from x = into @String (version x) <> case revision x of
        Nothing -> ""
        Just r -> "-" <> into @String r

instance ToXml.ToXml Release where
    toXml = ToXml.toXml . into @String
