module Monadoc.Type.GithubUser where

import Monadoc.Prelude

import qualified Data.Aeson as Aeson
import qualified Monadoc.Type.GithubId as GithubId
import qualified Monadoc.Type.GithubLogin as GithubLogin

data GithubUser = GithubUser
    { id :: GithubId.GithubId
    , login :: GithubLogin.GithubLogin
    } deriving (Eq, Show)

instance Aeson.FromJSON GithubUser where
    parseJSON = Aeson.withObject "GithubUser" $ \ object -> do
        i <- object Aeson..: into @Text "id"
        l <- object Aeson..: into @Text "login"
        pure GithubUser
            { id = i
            , login = l
            }
