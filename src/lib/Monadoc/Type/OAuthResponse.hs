module Monadoc.Type.OAuthResponse where

import Monadoc.Prelude

import qualified Data.Aeson as Aeson
import qualified Witch

data OAuthResponse = OAuthResponse
    { accessToken :: String
    , tokenType :: String
    } deriving (Eq, Show)

instance Aeson.FromJSON OAuthResponse where
    parseJSON = Aeson.withObject "OAuthResponse" $ \ object -> do
        at <- object Aeson..: Witch.into @Text "access_token"
        tt <- object Aeson..: Witch.into @Text "token_type"
        pure $ OAuthResponse { accessToken = at, tokenType = tt }
