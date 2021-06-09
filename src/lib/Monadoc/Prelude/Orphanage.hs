{-# OPTIONS_GHC -Wno-orphans #-}

module Monadoc.Prelude.Orphanage where

import qualified Data.Text
import qualified Database.SQLite.Simple
import qualified Distribution.Utils.ShortText
import qualified Prelude
import qualified Witch

instance Witch.From Database.SQLite.Simple.Query Prelude.String where
    from = Witch.via @Data.Text.Text

instance Witch.From Database.SQLite.Simple.Query Data.Text.Text

instance Witch.From Prelude.String Database.SQLite.Simple.Query where
    from = Witch.via @Data.Text.Text

instance Witch.From Data.Text.Text Database.SQLite.Simple.Query

instance Witch.From Prelude.String Distribution.Utils.ShortText.ShortText where
    from = Distribution.Utils.ShortText.toShortText

instance Witch.From Distribution.Utils.ShortText.ShortText Prelude.String where
    from = Distribution.Utils.ShortText.fromShortText

instance Witch.From Data.Text.Text Distribution.Utils.ShortText.ShortText where
    from = Witch.via @Prelude.String

instance Witch.From Distribution.Utils.ShortText.ShortText Data.Text.Text where
    from = Witch.via @Prelude.String
