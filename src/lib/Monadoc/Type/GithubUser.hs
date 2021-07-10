{-# LANGUAGE TypeApplications #-}

module Monadoc.Type.GithubUser where

import Monadoc.Prelude

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Monadoc.Type.GithubId as GithubId
import qualified Monadoc.Type.GithubLogin as GithubLogin
import qualified Witch

data GithubUser = GithubUser
    { id_ :: GithubId.GithubId
    , login :: GithubLogin.GithubLogin
    } deriving (Eq, Show)

instance Aeson.FromJSON GithubUser where
    parseJSON = Aeson.withObject "GithubUser" $ \ object -> do
        i <- object Aeson..: Witch.into @Text.Text "id"
        l <- object Aeson..: Witch.into @Text.Text "login"
        pure GithubUser
            { id_ = i
            , login = l
            }
