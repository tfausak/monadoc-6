module Monadoc.Type.GithubUser where

import qualified Data.Aeson as Aeson
import qualified Monadoc.Type.GithubId as GithubId
import qualified Monadoc.Type.GithubLogin as GithubLogin
import qualified Monadoc.Utility.Aeson as Aeson

data GithubUser = GithubUser
    { id_ :: GithubId.GithubId
    , login :: GithubLogin.GithubLogin
    } deriving (Eq, Show)

instance Aeson.FromJSON GithubUser where
    parseJSON = Aeson.withObject "GithubUser" $ \ object -> do
        i <- Aeson.required object "id"
        l <- Aeson.required object "login"
        pure GithubUser
            { id_ = i
            , login = l
            }
