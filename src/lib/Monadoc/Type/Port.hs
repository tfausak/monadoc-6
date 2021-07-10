{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Monadoc.Type.Port where

import Monadoc.Prelude

import qualified Network.Wai.Handler.Warp as Warp
import qualified Text.Read as Read

newtype Port
    = Port Warp.Port
    deriving (Eq, Show)

instance From Warp.Port Port

instance From Port Warp.Port

instance TryFrom String Port where
    tryFrom = maybeTryFrom $ fmap (from @Warp.Port) . Read.readMaybe
