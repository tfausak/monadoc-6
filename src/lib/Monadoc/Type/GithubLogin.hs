{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Type.GithubLogin where

import Monadoc.Prelude

import qualified Data.Aeson as Aeson
import qualified Monadoc.Class.ToXml as ToXml
import qualified Monadoc.Vendor.Sql as Sql
import qualified Witch

newtype GithubLogin
    = GithubLogin String
    deriving (Eq, Show)

instance Witch.From String GithubLogin

instance Witch.From GithubLogin String

instance Sql.FromField GithubLogin where
    fromField = fmap (Witch.from @String) . Sql.fromField

instance Sql.ToField GithubLogin where
    toField = Sql.toField . Witch.into @String

instance Aeson.FromJSON GithubLogin where
    parseJSON = fmap (Witch.from @String) . Aeson.parseJSON

instance ToXml.ToXml GithubLogin where
    toXml = ToXml.toXml . Witch.into @String
