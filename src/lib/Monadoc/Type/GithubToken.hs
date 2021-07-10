{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Type.GithubToken where

import Monadoc.Prelude

import qualified Data.Aeson as Aeson
import qualified Monadoc.Vendor.Sql as Sql

newtype GithubToken
    = GithubToken String
    deriving (Eq, Show)

instance From String GithubToken

instance From GithubToken String

instance Sql.FromField GithubToken where
    fromField = fmap (from @String) . Sql.fromField

instance Sql.ToField GithubToken where
    toField = Sql.toField . into @String

instance Aeson.FromJSON GithubToken where
    parseJSON = fmap (from @String) . Aeson.parseJSON
