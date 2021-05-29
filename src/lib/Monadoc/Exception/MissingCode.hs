module Monadoc.Exception.MissingCode where

import Monadoc.Prelude

import qualified Control.Monad.Catch as Exception
import qualified Network.Wai as Wai

newtype MissingCode
    = MissingCode Wai.Request
    deriving Show

instance Exception.Exception MissingCode where
    displayException = ("missing OAuth code: " <>) <<< show <<< into @Wai.Request

instance From Wai.Request MissingCode

instance From MissingCode Wai.Request
