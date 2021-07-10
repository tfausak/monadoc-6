{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Type.GithubToken where

import qualified Data.Aeson as Aeson
import qualified Monadoc.Vendor.Sql as Sql
import qualified Witch

newtype GithubToken
    = GithubToken String
    deriving (Eq, Show)

instance Witch.From String GithubToken

instance Witch.From GithubToken String

instance Sql.FromField GithubToken where
    fromField = fmap (Witch.from @String) . Sql.fromField

instance Sql.ToField GithubToken where
    toField = Sql.toField . Witch.into @String

instance Aeson.FromJSON GithubToken where
    parseJSON = fmap (Witch.from @String) . Aeson.parseJSON
