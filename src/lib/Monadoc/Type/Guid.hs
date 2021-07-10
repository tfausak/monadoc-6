{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Type.Guid where

import Monadoc.Prelude

import qualified Data.Proxy as Proxy
import qualified Data.UUID as Uuid
import qualified Monadoc.Class.ToXml as ToXml
import qualified Monadoc.Vendor.Sql as Sql
import qualified System.Random.Stateful as Random

newtype Guid
    = Guid Uuid.UUID
    deriving (Eq, Show)

instance Sql.FromField Guid where
    fromField = Sql.defaultFromField @Text Proxy.Proxy

instance Sql.ToField Guid where
    toField = Sql.toField . into @Text

instance Random.Uniform Guid where
    uniformM = fmap (from @Uuid.UUID) . Random.uniformM

instance From Uuid.UUID Guid

instance From Guid Uuid.UUID

instance From Guid Text where
    from = Uuid.toText . into @Uuid.UUID

instance TryFrom Text Guid where
    tryFrom = maybeTryFrom $ fmap (from @Uuid.UUID) . Uuid.fromText

instance ToXml.ToXml Guid where
    toXml = ToXml.toXml . into @Text

random :: IO Guid
random = Random.getStdRandom Random.uniform
