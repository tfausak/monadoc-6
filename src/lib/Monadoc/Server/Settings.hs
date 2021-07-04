module Monadoc.Server.Settings where

import Monadoc.Prelude

import qualified Control.Exception as Exception
import qualified Data.Typeable as Typeable
import qualified Monadoc.Exception.Forbidden as Forbidden
import qualified Monadoc.Exception.Found as Found
import qualified Monadoc.Exception.NotFound as NotFound
import qualified Monadoc.Server.Response as Response
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.RequestId as RequestId
import qualified Monadoc.Type.Version as Version
import qualified Monadoc.Utility.Log as Log
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Paths_monadoc as This
import qualified Text.Printf as Printf

fromConfig :: Config.Config -> Warp.Settings
fromConfig config = Warp.defaultSettings
    & Warp.setBeforeMainLoop (beforeMainLoop config)
    & Warp.setHost (Config.host config)
    & Warp.setOnException onException
    & Warp.setOnExceptionResponse onExceptionResponse
    & Warp.setPort (into @Warp.Port $ Config.port config)
    & Warp.setServerName serverName

beforeMainLoop :: Config.Config -> IO ()
beforeMainLoop config = Log.info $ "[server] listening on port " <> show (Config.port config)

onException :: Maybe Wai.Request -> SomeException -> IO ()
onException maybeRequest (SomeException e) = Log.warn $ Printf.printf
    "[exception/%s] [%04x] %s"
    (show $ Typeable.typeOf e)
    (maybe 0 (maybe 0 (into @Word16) . RequestId.get) maybeRequest)
    (displayException e)

onExceptionResponse :: SomeException -> Wai.Response
onExceptionResponse e
    | Just (Found.Found location) <- Exception.fromException e =
        Response.status Http.found302 [(Http.hLocation, into @ByteString location)]
    | Just Forbidden.Forbidden <- Exception.fromException e =
        Response.status Http.forbidden403 []
    | Just NotFound.NotFound <- Exception.fromException e =
        Response.status Http.notFound404 []
    | otherwise =
        Response.status Http.internalServerError500 []

serverName :: ByteString
serverName = into @ByteString
    $ "monadoc/" <> into @String (into @Version.Version This.version)
