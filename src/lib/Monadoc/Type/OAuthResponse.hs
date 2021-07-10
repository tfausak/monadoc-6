{-# LANGUAGE TypeApplications #-}

module Monadoc.Type.OAuthResponse where

import Monadoc.Prelude

import qualified Data.Aeson as Aeson
import qualified Monadoc.Type.GithubToken as GithubToken

data OAuthResponse = OAuthResponse
    { accessToken :: GithubToken.GithubToken
    , tokenType :: String
    } deriving (Eq, Show)

instance Aeson.FromJSON OAuthResponse where
    parseJSON = Aeson.withObject "OAuthResponse" $ \ object -> do
        at <- object Aeson..: into @Text "access_token"
        tt <- object Aeson..: into @Text "token_type"
        pure OAuthResponse { accessToken = at, tokenType = tt }
