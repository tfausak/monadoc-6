module Monadoc.Type.GithubUser where

import Monadoc.Prelude

import qualified Data.Aeson as Aeson
import qualified Monadoc.Utility.Convert as Convert

data GithubUser = GithubUser
    { id_ :: Int
    , login :: String
    } deriving (Eq, Show)

instance Aeson.FromJSON GithubUser where
    parseJSON = Aeson.withObject "GithubUser" $ \ object -> do
        i <- object Aeson..: Convert.stringToText "id"
        l <- object Aeson..: Convert.stringToText "login"
        pure GithubUser
            { id_ = i
            , login = l
            }
