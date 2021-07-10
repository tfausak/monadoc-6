{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Monadoc.Type.GithubLogin where

import Monadoc.Prelude

import qualified Data.Aeson as Aeson
import qualified Monadoc.Class.ToXml as ToXml
import qualified Monadoc.Vendor.Sql as Sql

newtype GithubLogin
    = GithubLogin String
    deriving (Eq, Show)

instance From String GithubLogin

instance From GithubLogin String

instance Sql.FromField GithubLogin where
    fromField = fmap (from @String) . Sql.fromField

instance Sql.ToField GithubLogin where
    toField = Sql.toField . into @String

instance Aeson.FromJSON GithubLogin where
    parseJSON = fmap (from @String) . Aeson.parseJSON

instance ToXml.ToXml GithubLogin where
    toXml = ToXml.toXml . into @String
