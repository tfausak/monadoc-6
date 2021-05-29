module Monadoc.Type.GithubUser where

import Monadoc.Prelude

import qualified Data.Aeson as Aeson
import qualified Witch

data GithubUser = GithubUser
    { id_ :: Int
    , login :: String
    } deriving (Eq, Show)

instance Aeson.FromJSON GithubUser where
    parseJSON = Aeson.withObject "GithubUser" $ \ object -> do
        i <- object Aeson..: Witch.into @Text "id"
        l <- object Aeson..: Witch.into @Text "login"
        pure GithubUser
            { id_ = i
            , login = l
            }
