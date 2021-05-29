{-# OPTIONS_GHC -Wno-orphans #-}

module Monadoc.Prelude.Orphanage where

import qualified Data.Text
import qualified Database.SQLite.Simple
import qualified Prelude
import qualified Witch

instance Witch.From Database.SQLite.Simple.Query Prelude.String where
    from = Witch.via @Data.Text.Text

instance Witch.From Database.SQLite.Simple.Query Data.Text.Text

instance Witch.From Prelude.String Database.SQLite.Simple.Query where
    from = Witch.via @Data.Text.Text

instance Witch.From Data.Text.Text Database.SQLite.Simple.Query
