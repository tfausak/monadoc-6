{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Type.Revision where

import qualified Monadoc.Vendor.Sql as Sql
import qualified Monadoc.Class.ToXml as ToXml
import qualified Text.Read as Read
import qualified Witch

newtype Revision
    = Revision Word
    deriving (Eq, Ord, Show)

instance Witch.From Word Revision

instance Witch.From Revision Word

instance Sql.FromField Revision where
    fromField = fmap (Witch.from @Word) . Sql.fromField

instance Sql.ToField Revision where
    toField = Sql.toField . Witch.into @Word

instance ToXml.ToXml Revision where
    toXml = ToXml.toXml . Witch.into @Word

instance Witch.TryFrom String Revision where
    tryFrom = Witch.maybeTryFrom $ fmap (Witch.from @Word) . Read.readMaybe

instance Witch.From Revision String where
    from = show . Witch.into @Word

zero :: Revision
zero = Witch.from @Word 0

increment :: Revision -> Revision
increment = Witch.over @Word (+ 1)

decrement :: Revision -> Maybe Revision
decrement x = if x == zero then Nothing else Just $ Witch.over @Word (subtract 1) x
