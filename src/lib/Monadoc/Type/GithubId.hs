module Monadoc.Type.GithubId where

import Monadoc.Prelude

import qualified Data.Aeson as Aeson
import qualified Monadoc.Vendor.Sql as Sql

newtype GithubId
    = GithubId Int
    deriving (Eq, Show)

instance From Int GithubId

instance From GithubId Int

instance Sql.FromField GithubId where
    fromField = fmap (from @Int) . Sql.fromField

instance Sql.ToField GithubId where
    toField = Sql.toField . into @Int

instance Aeson.FromJSON GithubId where
    parseJSON = fmap (from @Int) . Aeson.parseJSON
