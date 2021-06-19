module Monadoc.Utility.Log where

import Monadoc.Prelude

import qualified Control.Concurrent.STM as Stm
import qualified Data.Time as Time
import qualified Monadoc.Utility.Convert as Convert
import qualified System.IO as IO
import qualified System.IO.Unsafe as Unsafe

info :: String -> IO ()
info = onHandle IO.stdout

warn :: String -> IO ()
warn = onHandle IO.stderr

onHandle :: IO.Handle -> String -> IO ()
onHandle h message = do
    now <- Time.getCurrentTime
    withLock . IO.hPutStrLn h $ Convert.timeToString now <> " " <> message

withLock :: IO a -> IO a
withLock = bracket
    (Stm.atomically $ Stm.takeTMVar lock)
    (Stm.atomically . Stm.putTMVar lock)
    . always

lock :: Stm.TMVar ()
lock = Unsafe.unsafePerformIO $ Stm.newTMVarIO ()
{-# noinline lock #-}
