module Monadoc.Type.Guid where

import Monadoc.Prelude

import qualified Data.UUID as Uuid
import qualified Database.SQLite.Simple as Sql
import qualified Database.SQLite.Simple.FromField as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import qualified System.Random.Stateful as Random

newtype Guid
    = Guid Uuid.UUID
    deriving (Eq, Show)

instance Sql.FromField Guid where
    fromField field = do
        text <- Sql.fromField field
        case Uuid.fromText text of
            Nothing -> Sql.returnError Sql.ConversionFailed field "invalid Guid"
            Just uuid -> pure $ from @Uuid.UUID uuid

instance Sql.ToField Guid where
    toField = Sql.toField . Uuid.toText . into @Uuid.UUID

instance Random.Uniform Guid where
    uniformM = fmap (from @Uuid.UUID) . Random.uniformM

instance From Uuid.UUID Guid

instance From Guid Uuid.UUID

random :: IO Guid
random = Random.getStdRandom Random.uniform
