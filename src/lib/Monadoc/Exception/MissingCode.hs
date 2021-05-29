module Monadoc.Exception.MissingCode where

import Monadoc.Prelude

import qualified Control.Monad.Catch as Exception
import qualified Network.Wai as Wai
import qualified Witch

newtype MissingCode
    = MissingCode_ Wai.Request
    deriving Show

instance Exception.Exception MissingCode where
    displayException = ("missing OAuth code: " <>) . show . Witch.into @Wai.Request

instance Witch.From Wai.Request MissingCode

instance Witch.From MissingCode Wai.Request
