{-# OPTIONS_GHC -Wno-orphans #-}

module Monadoc.Prelude.Orphanage where

import qualified Prelude
import qualified Data.ByteString
import qualified Data.Text
import qualified Database.SQLite.Simple
import qualified Witch

instance Witch.TryFrom Data.ByteString.ByteString Prelude.String where
    tryFrom = Witch.eitherTryFrom (\ s -> Prelude.fmap (Witch.into @Prelude.String) (Witch.tryInto @Data.Text.Text s))

instance Witch.From Database.SQLite.Simple.Query Prelude.String where
    from = Witch.via @Data.Text.Text

instance Witch.From Database.SQLite.Simple.Query Data.Text.Text

instance Witch.From Prelude.String Data.ByteString.ByteString where
    from = Witch.via @Data.Text.Text

instance Witch.From Prelude.String Database.SQLite.Simple.Query where
    from = Witch.via @Data.Text.Text

instance Witch.From Data.Text.Text Database.SQLite.Simple.Query
