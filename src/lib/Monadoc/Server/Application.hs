module Monadoc.Server.Application where

import Monadoc.Prelude

import qualified Monadoc.Exception.NotFound as NotFound
import qualified Monadoc.Handler.GetAccount as GetAccount
import qualified Monadoc.Handler.GetFavicon as GetFavicon
import qualified Monadoc.Handler.GetCallback as GetCallback
import qualified Monadoc.Handler.GetIndex as GetIndex
import qualified Monadoc.Handler.GetPackage as GetPackage
import qualified Monadoc.Handler.GetRevision as GetRevision
import qualified Monadoc.Handler.GetSearch as GetSearch
import qualified Monadoc.Handler.GetVersion as GetVersion
import qualified Monadoc.Handler.PostLogOut as PostLogOut
import qualified Monadoc.Handler.PostRevoke as PostRevoke
import qualified Monadoc.Server.Response as Response
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Handler as Handler
import qualified Monadoc.Type.Route as Route
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified System.FilePath as FilePath

application :: Context.Context -> Wai.Application
application context request respond = do
    handler <- case getHandler request of
        Nothing -> throwM NotFound.new
        Just handler -> pure handler
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
        (Http.GET, Route.Callback) -> Just GetCallback.handler
        (Http.GET, Route.Logo) -> Just $ fileHandler "monadoc.svg" "image/svg+xml; charset=UTF-8"
        (Http.GET, Route.Template) -> Just $ fileHandler "monadoc.xsl" "text/xsl; charset=UTF-8"
        (Http.GET, Route.Robots) -> Just $ fileHandler "robots.txt" "text/plain; charset=UTF-8"
        (Http.GET, Route.Package packageName) -> Just $ GetPackage.handler packageName
        (Http.GET, Route.Version packageName version) -> Just $ GetVersion.handler packageName version
        (Http.GET, Route.Revision packageName version revision) -> Just $ GetRevision.handler packageName version revision
        (Http.GET, Route.Search query) -> Just $ GetSearch.handler query
        (Http.GET, Route.Account) -> Just GetAccount.handler
        (Http.POST, Route.LogOut) -> Just PostLogOut.handler
        (Http.POST, Route.Revoke) -> Just PostRevoke.handler
        _ -> Nothing

getMethod :: Wai.Request -> Maybe Http.StdMethod
getMethod = either (always Nothing) Just . Http.parseMethod . Wai.requestMethod

getRoute :: Wai.Request -> Maybe Route.Route
getRoute request =
    let
        path = fmap (into @String) $ Wai.pathInfo request
        query = Wai.queryString request
    in Route.parse path query

fileHandler :: FilePath -> String -> Handler.Handler
fileHandler relative mime context _ = do
    let
        status = Http.ok200
        headers = [(Http.hContentType, into @ByteString mime)]
        config = Context.config context
        directory = Config.dataDirectory config
        absolute = FilePath.combine directory relative
    Response.file status headers absolute
