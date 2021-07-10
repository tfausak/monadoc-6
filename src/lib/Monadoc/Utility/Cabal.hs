module Monadoc.Utility.Cabal where

import qualified Control.Monad.Catch as Exception
import qualified Data.Proxy as Proxy
import qualified Distribution.Parsec as Cabal
import qualified Witch

parsecTryFrom
    :: (Cabal.Parsec a, Witch.From a b)
    => proxy a
    -> String
    -> Either (Witch.TryFromException String b) b
parsecTryFrom proxy string = case Cabal.eitherParsec string of
    Left message -> Left
        . Witch.TryFromException string
        . Just
        . Exception.toException
        $ userError message
    Right a -> Right . Witch.from $ Proxy.asProxyTypeOf a proxy
