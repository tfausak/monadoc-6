{-# LANGUAGE TypeApplications #-}

module Monadoc.Type.OAuthResponse where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Monadoc.Type.GithubToken as GithubToken
import qualified Witch

data OAuthResponse = OAuthResponse
    { accessToken :: GithubToken.GithubToken
    , tokenType :: String
    } deriving (Eq, Show)

instance Aeson.FromJSON OAuthResponse where
    parseJSON = Aeson.withObject "OAuthResponse" $ \ object -> do
        at <- object Aeson..: Witch.into @Text.Text "access_token"
        tt <- object Aeson..: Witch.into @Text.Text "token_type"
        pure OAuthResponse { accessToken = at, tokenType = tt }
