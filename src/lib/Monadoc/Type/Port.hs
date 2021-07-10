{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Type.Port where

import Monadoc.Prelude

import qualified Network.Wai.Handler.Warp as Warp
import qualified Text.Read as Read
import qualified Witch

newtype Port
    = Port Warp.Port
    deriving (Eq, Show)

instance Witch.From Warp.Port Port

instance Witch.From Port Warp.Port

instance Witch.TryFrom String Port where
    tryFrom = Witch.maybeTryFrom $ fmap (Witch.from @Warp.Port) . Read.readMaybe
