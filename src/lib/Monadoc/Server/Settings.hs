module Monadoc.Server.Settings where

import qualified Control.Monad.Catch as Exception
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Version as Version
import qualified Monadoc.Log as Log
import qualified Monadoc.Type.Config as Config
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Paths_monadoc as Package
import qualified System.IO as IO

fromConfig :: Config.Config -> Warp.Settings
fromConfig config = Warp.setBeforeMainLoop (beforeMainLoop config)
    . Warp.setHost (Config.host config)
    . Warp.setOnException onException
    . Warp.setOnExceptionResponse onExceptionResponse
    . Warp.setPort (Config.port config)
    $ Warp.setServerName serverName Warp.defaultSettings

beforeMainLoop :: Config.Config -> IO ()
beforeMainLoop config = Log.info $ "listening on port " <> show (Config.port config)

onException :: Maybe Wai.Request -> Exception.SomeException -> IO ()
onException _ = IO.hPutStrLn IO.stderr . Exception.displayException

onExceptionResponse :: Exception.SomeException -> Wai.Response
onExceptionResponse _ = Wai.responseLBS
    Http.internalServerError500
    [(Http.hContentType, toUtf8 "text/plain; charset=utf-8")]
    . LazyByteString.fromStrict $ toUtf8 "500 Internal Server Error"

serverName :: ByteString.ByteString
serverName = Text.encodeUtf8 . Text.pack
    $ "monadoc/" <> Version.showVersion Package.version

toUtf8 :: String -> ByteString.ByteString
toUtf8 = Text.encodeUtf8 . Text.pack
