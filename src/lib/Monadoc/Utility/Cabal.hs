module Monadoc.Utility.Cabal where

import qualified Control.Monad.Catch as Exception
import qualified Data.Proxy as Proxy
import qualified Distribution.Parsec as Cabal
import qualified Witch

parsecTryFrom
    :: (Cabal.Parsec a, Witch.From a b)
    => Proxy.Proxy a
    -> String
    -> Either (Witch.TryFromException String b) b
parsecTryFrom proxy string = case Cabal.eitherParsec string of
    Left message -> Left
        . Witch.TryFromException string
        . Just
        . Exception.toException
        $ userError message
    Right x -> Right . Witch.from $ Proxy.asProxyTypeOf x proxy
