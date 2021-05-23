module Monadoc.Server.Application where

import qualified Monadoc.Handler.GetFavicon as GetFavicon
import qualified Monadoc.Handler.GetGithubCallback as GetGithubCallback
import qualified Monadoc.Handler.GetIndex as GetIndex
import qualified Monadoc.Server.Response as Response
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Handler as Handler
import qualified Monadoc.Utility.Convert as Convert
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified System.FilePath as FilePath

application :: Context.Context -> Wai.Application
application context request respond = do
    let
        method = Convert.utf8ToString $ Wai.requestMethod request
        path = fmap Convert.textToString $ Wai.pathInfo request
        handler = case (method, path) of
            ("GET", []) -> GetIndex.handler
            ("GET", ["bootstrap.css"]) -> fileHandler "bootstrap.css" "text/css; charset=UTF-8"
            ("GET", ["favicon.ico"]) -> GetFavicon.handler
            ("GET", ["github-callback"]) -> GetGithubCallback.handler
            ("GET", ["monadoc.svg"]) -> fileHandler "monadoc.svg" "image/svg+xml; charset=UTF-8"
            ("GET", ["monadoc.xsl"]) -> fileHandler "monadoc.xsl" "text/xsl; charset=UTF-8"
            ("GET", ["robots.txt"]) -> fileHandler "robots.txt" "text/plain; charset=UTF-8"
            _ -> notFoundHandler
    response <- handler context request
    respond response

fileHandler :: FilePath -> String -> Handler.Handler
fileHandler relative mime context _ = do
    let
        status = Http.ok200
        headers = [(Http.hContentType, Convert.stringToUtf8 mime)]
        config = Context.config context
        directory = Config.dataDirectory config
        absolute = FilePath.combine directory relative
    Response.file status headers absolute

notFoundHandler :: Handler.Handler
notFoundHandler _ _ = pure $ Response.status Http.notFound404 []
