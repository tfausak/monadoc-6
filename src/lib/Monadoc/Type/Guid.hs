module Monadoc.Type.Guid where

import Monadoc.Prelude

import qualified Data.UUID as Uuid
import qualified Database.SQLite.Simple as Sql
import qualified Database.SQLite.Simple.FromField as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import qualified Monadoc.Class.ToXml as ToXml
import qualified System.Random.Stateful as Random

newtype Guid
    = Guid Uuid.UUID
    deriving (Eq, Show)

instance Sql.FromField Guid where
    fromField field = do
        text <- Sql.fromField field
        case tryFrom @Text text of
            Left _ -> Sql.returnError Sql.ConversionFailed field "invalid Guid"
            Right guid -> pure guid

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
