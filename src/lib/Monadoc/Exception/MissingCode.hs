module Monadoc.Exception.MissingCode where

import Monadoc.Prelude

import qualified Network.Wai as Wai

newtype MissingCode
    = MissingCode Wai.Request
    deriving Show

instance Exception MissingCode where
    displayException = sappend "missing OAuth code: " <. show <. into @Wai.Request

instance From Wai.Request MissingCode

instance From MissingCode Wai.Request
