module Monadoc.Server.Settings where

import Monadoc.Prelude

import qualified Control.Monad.Catch as Exception
import qualified Data.Typeable as Typeable
import qualified Monadoc.Server.Response as Response
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Utility.Convert as Convert
import qualified Monadoc.Utility.Log as Log
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Paths_monadoc as Package
import qualified Witch

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
onException _ (Exception.SomeException e) = Log.warn $
    "[" <> show (Typeable.typeOf e) <> "] " <> Exception.displayException e

onExceptionResponse :: Exception.SomeException -> Wai.Response
onExceptionResponse _ = Response.status Http.internalServerError500 []

serverName :: ByteString
serverName = Witch.into @ByteString
    $ "monadoc/" <> Convert.versionToString Package.version
