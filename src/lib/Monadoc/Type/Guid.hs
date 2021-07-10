{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Type.Guid where

import Monadoc.Prelude

import qualified Data.Proxy as Proxy
import qualified Data.Text as Text
import qualified Data.UUID as Uuid
import qualified Monadoc.Class.ToXml as ToXml
import qualified Monadoc.Vendor.Sql as Sql
import qualified System.Random.Stateful as Random
import qualified Witch

newtype Guid
    = Guid Uuid.UUID
    deriving (Eq, Show)

instance Sql.FromField Guid where
    fromField = Sql.defaultFromField @Text.Text Proxy.Proxy

instance Sql.ToField Guid where
    toField = Sql.toField . Witch.into @Text.Text

instance Random.Uniform Guid where
    uniformM = fmap (Witch.from @Uuid.UUID) . Random.uniformM

instance Witch.From Uuid.UUID Guid

instance Witch.From Guid Uuid.UUID

instance Witch.From Guid Text.Text where
    from = Uuid.toText . Witch.into @Uuid.UUID

instance Witch.TryFrom Text.Text Guid where
    tryFrom = Witch.maybeTryFrom $ fmap (Witch.from @Uuid.UUID) . Uuid.fromText

instance ToXml.ToXml Guid where
    toXml = ToXml.toXml . Witch.into @Text.Text

random :: IO Guid
random = Random.getStdRandom Random.uniform
