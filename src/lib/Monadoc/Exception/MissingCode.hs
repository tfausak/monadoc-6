module Monadoc.Exception.MissingCode where

import Monadoc.Prelude

import qualified Network.Wai as Wai

newtype MissingCode
    = MissingCode Wai.Request
    deriving Show

instance Exception MissingCode

new :: Wai.Request -> MissingCode
new = MissingCode
