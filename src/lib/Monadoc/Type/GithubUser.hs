module Monadoc.Type.GithubUser where

import Monadoc.Prelude

import qualified Data.Aeson as Aeson

data GithubUser = GithubUser
    { id :: Int
    , login :: String
    } deriving (Eq, Show)

instance Aeson.FromJSON GithubUser where
    parseJSON = Aeson.withObject "GithubUser" <| \ object -> do
        id <- object Aeson..: into @Text "id"
        login <- object Aeson..: into @Text "login"
        pure GithubUser
            { id
            , login
            }
