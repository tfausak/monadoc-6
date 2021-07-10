{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Type.Release where

import Monadoc.Prelude

import qualified Monadoc.Class.ToXml as ToXml
import qualified Monadoc.Type.Revision as Revision
import qualified Monadoc.Type.Version as Version
import qualified Monadoc.Utility.Either as Either
import qualified Witch

data Release = Release
    { version :: Version.Version
    , revision :: Maybe Revision.Revision
    } deriving (Eq, Show)

instance Witch.TryFrom String Release where
    tryFrom = Witch.maybeTryFrom $ \ string -> do
        let (before, after) = break (== '-') string
        v <- Either.toMaybe $ Witch.tryFrom @String before
        case after of
            "" -> pure Release { version = v, revision = Nothing }
            '-' : rest -> do
                r <- Either.toMaybe $ Witch.tryFrom @String rest
                pure Release { version = v, revision = Just r }
            _ -> Nothing

instance Witch.From Release String where
    from x = Witch.into @String (version x) <> case revision x of
        Nothing -> ""
        Just r -> "-" <> Witch.into @String r

instance ToXml.ToXml Release where
    toXml = ToXml.toXml . Witch.into @String
