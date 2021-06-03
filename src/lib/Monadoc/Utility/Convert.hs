module Monadoc.Utility.Convert where

import Monadoc.Prelude

import qualified Data.String as String
import qualified Data.Time as Time
import qualified Network.Wai.Handler.Warp as Warp

stringToHost :: String -> Warp.HostPreference
stringToHost = String.fromString

timeToString :: Time.UTCTime -> String
timeToString = Time.formatTime Time.defaultTimeLocale "%Y-%m-%dT%H:%M:%S%3QZ"
