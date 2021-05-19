module Monadoc.Log where

import qualified Data.Time as Time
import qualified Monadoc.Convert as Convert
import qualified System.IO as IO

info :: String -> IO ()
info message = do
    now <- Time.getCurrentTime
    putStrLn $ Convert.timeToString now <> " " <> message

warn :: String -> IO ()
warn message = do
    now <- Time.getCurrentTime
    IO.hPutStrLn IO.stderr $ Convert.timeToString now <> " " <> message
