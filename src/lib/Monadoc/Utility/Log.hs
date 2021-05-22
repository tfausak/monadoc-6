module Monadoc.Utility.Log where

import qualified Control.Concurrent.STM as Stm
import qualified Control.Monad.Catch as Exception
import qualified Data.Time as Time
import qualified Monadoc.Utility.Convert as Convert
import qualified System.IO as IO
import qualified System.IO.Unsafe as Unsafe

info :: String -> IO ()
info = onHandle IO.stdout

warn :: String -> IO ()
warn = onHandle IO.stderr

onHandle :: IO.Handle -> String -> IO ()
onHandle handle message = do
    now <- Time.getCurrentTime
    withLock . IO.hPutStrLn handle $ Convert.timeToString now <> " " <> message

withLock :: IO a -> IO a
withLock action = Exception.bracket
    (Stm.atomically $ Stm.takeTMVar lock)
    (Stm.atomically . Stm.putTMVar lock)
    (\ () -> action)

lock :: Stm.TMVar ()
lock = Unsafe.unsafePerformIO $ Stm.newTMVarIO ()
{-# noinline lock #-}
