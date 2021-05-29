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
        id_ <- object Aeson..: Witch.into @Text "id"
        login <- object Aeson..: Witch.into @Text "login"
        pure GithubUser
            { id_
            , login
            }
