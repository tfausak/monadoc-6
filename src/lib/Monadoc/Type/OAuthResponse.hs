module Monadoc.Type.OAuthResponse where

import qualified Data.Aeson as Aeson
import qualified Monadoc.Type.GithubToken as GithubToken
import qualified Monadoc.Utility.Aeson as Aeson

data OAuthResponse = OAuthResponse
    { accessToken :: GithubToken.GithubToken
    , tokenType :: String
    } deriving (Eq, Show)

instance Aeson.FromJSON OAuthResponse where
    parseJSON = Aeson.withObject "OAuthResponse" $ \ object -> do
        at <- Aeson.required object "access_token"
        tt <- Aeson.required object "token_type"
        pure OAuthResponse { accessToken = at, tokenType = tt }
