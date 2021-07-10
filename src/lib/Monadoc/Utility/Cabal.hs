module Monadoc.Utility.Cabal where

import Monadoc.Prelude

import qualified Control.Monad.Catch as Exception
import qualified Data.Proxy as Proxy
import qualified Distribution.Parsec as Cabal

parsecTryFrom
    :: (Cabal.Parsec a, From a b)
    => Proxy a
    -> String
    -> Either (TryFromException String b) b
parsecTryFrom proxy string = case Cabal.eitherParsec string of
    Left message -> Left
        . TryFromException string
        . Just
        . Exception.toException
        $ userError message
    Right x -> Right . from $ Proxy.asProxyTypeOf x proxy
