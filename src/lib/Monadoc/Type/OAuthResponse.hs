module Monadoc.Type.OAuthResponse where

import qualified Data.Aeson as Aeson
import qualified Monadoc.Utility.Convert as Convert

newtype OAuthResponse = OAuthResponse
    { accessToken :: String
    } deriving (Eq, Show)

instance Aeson.FromJSON OAuthResponse where
    parseJSON = Aeson.withObject "OAuthResponse" $ \ object -> do
        t <- object Aeson..: Convert.stringToText "access_token"
        pure $ OAuthResponse { accessToken = t }
