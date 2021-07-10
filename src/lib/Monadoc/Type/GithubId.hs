{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Type.GithubId where

import qualified Data.Aeson as Aeson
import qualified Monadoc.Vendor.Sql as Sql
import qualified Witch

newtype GithubId
    = GithubId Int
    deriving (Eq, Show)

instance Witch.From Int GithubId

instance Witch.From GithubId Int

instance Sql.FromField GithubId where
    fromField = fmap (Witch.from @Int) . Sql.fromField

instance Sql.ToField GithubId where
    toField = Sql.toField . Witch.into @Int

instance Aeson.FromJSON GithubId where
    parseJSON = fmap (Witch.from @Int) . Aeson.parseJSON
