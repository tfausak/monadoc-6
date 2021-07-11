module Monadoc.Utility.Convert where

import qualified Data.String as String
import qualified Network.Wai.Handler.Warp as Warp

stringToHost :: String -> Warp.HostPreference
stringToHost = String.fromString
