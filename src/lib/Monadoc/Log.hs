module Monadoc.Log where

import qualified Data.Time as Time
import qualified Monadoc.Convert as Convert

info :: String -> IO ()
info message = do
    now <- Time.getCurrentTime
    putStrLn
        $ Convert.timeToString now
        <> " "
        <> message
