module Monadoc.Server.Settings where

import Monadoc.Prelude

import qualified Control.Exception as Exception
import qualified Data.Typeable as Typeable
import qualified Monadoc.Exception.Forbidden as Forbidden
import qualified Monadoc.Exception.NotFound as NotFound
import qualified Monadoc.Server.Response as Response
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Version as Version
import qualified Monadoc.Utility.Log as Log
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Paths_monadoc as This

fromConfig :: Config.Config -> Warp.Settings
fromConfig config = Warp.defaultSettings
    & Warp.setBeforeMainLoop (beforeMainLoop config)
    & Warp.setHost (Config.host config)
    & Warp.setOnException onException
    & Warp.setOnExceptionResponse onExceptionResponse
    & Warp.setPort (into @Warp.Port $ Config.port config)
    & Warp.setServerName serverName

beforeMainLoop :: Config.Config -> IO ()
beforeMainLoop config = Log.info $ "listening on port " <> show (Config.port config)

onException :: Maybe Wai.Request -> SomeException -> IO ()
onException _ (SomeException e) = Log.warn $
    "[" <> show (Typeable.typeOf e) <> "] " <> displayException e

onExceptionResponse :: SomeException -> Wai.Response
onExceptionResponse e
    | Just NotFound.NotFound <- Exception.fromException e = Response.status Http.notFound404 []
    | Just Forbidden.Forbidden <- Exception.fromException e = Response.status Http.forbidden403 []
    | otherwise = Response.status Http.internalServerError500 []

serverName :: ByteString
serverName = into @ByteString
    $ "monadoc/" <> into @String (into @Version.Version This.version)
