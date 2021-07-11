module Monadoc.Utility.Log where

import qualified Control.Concurrent.STM as Stm
import qualified Control.Monad.Catch as Exception
import qualified Data.Time as Time
import qualified System.IO as IO
import qualified System.IO.Unsafe as Unsafe

info :: String -> IO ()
info = onHandle IO.stdout

warn :: String -> IO ()
warn = onHandle IO.stderr

onHandle :: IO.Handle -> String -> IO ()
onHandle h message = do
    now <- Time.getCurrentTime
    let timestamp = Time.formatTime Time.defaultTimeLocale "%Y-%m-%dT%H:%M:%S%3QZ" now
    withLock . IO.hPutStrLn h $ timestamp <> " " <> message

withLock :: IO a -> IO a
withLock = Exception.bracket
    (Stm.atomically $ Stm.takeTMVar lock)
    (Stm.atomically . Stm.putTMVar lock)
    . const

lock :: Stm.TMVar ()
lock = Unsafe.unsafePerformIO $ Stm.newTMVarIO ()
{-# NOINLINE lock #-}
