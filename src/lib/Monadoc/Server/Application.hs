module Monadoc.Server.Application where

import Monadoc.Prelude

import qualified Data.Maybe as Maybe
import qualified Monadoc.Handler.GetFavicon as GetFavicon
import qualified Monadoc.Handler.GetGithubCallback as GetGithubCallback
import qualified Monadoc.Handler.GetIndex as GetIndex
import qualified Monadoc.Server.Response as Response
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Handler as Handler
import qualified Monadoc.Type.Route as Route
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified System.FilePath as FilePath
import qualified Witch

application :: Context.Context -> Wai.Application
application context request respond = do
    let handler = Maybe.fromMaybe notFoundHandler $ getHandler request
    response <- handler context request
    respond response

getHandler :: Wai.Request -> Maybe Handler.Handler
getHandler request = do
    method <- getMethod request
    route <- getRoute request
    case (method, route) of
        (Http.GET, Route.Index) -> Just GetIndex.handler
        (Http.GET, Route.Bootstrap) -> Just $ fileHandler "bootstrap.css" "text/css; charset=UTF-8"
        (Http.GET, Route.Favicon) -> Just GetFavicon.handler
        (Http.GET, Route.Callback) -> Just GetGithubCallback.handler
        (Http.GET, Route.Logo) -> Just $ fileHandler "monadoc.svg" "image/svg+xml; charset=UTF-8"
        (Http.GET, Route.Template) -> Just $ fileHandler "monadoc.xsl" "text/xsl; charset=UTF-8"
        (Http.GET, Route.Robots) -> Just $ fileHandler "robots.txt" "text/plain; charset=UTF-8"
        _ -> Nothing

getMethod :: Wai.Request -> Maybe Http.StdMethod
getMethod = either (\ _ -> Nothing) Just . Http.parseMethod . Wai.requestMethod

getRoute :: Wai.Request -> Maybe Route.Route
getRoute = Route.fromStrings . fmap (Witch.into @String) . Wai.pathInfo

fileHandler :: FilePath -> String -> Handler.Handler
fileHandler relative mime context _ = do
    let
        status = Http.ok200
        headers = [(Http.hContentType, Witch.into @ByteString mime)]
        config = Context.config context
        directory = Config.dataDirectory config
        absolute = FilePath.combine directory relative
    Response.file status headers absolute

notFoundHandler :: Handler.Handler
notFoundHandler _ _ = pure $ Response.status Http.notFound404 []
