module Monadoc.Server.Application where

import qualified Data.ByteString.Lazy as LazyByteString
import qualified Monadoc.Server.Response as Response
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Utility.Convert as Convert
import qualified Monadoc.Utility.Xml as Xml
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Paths_monadoc as Package
import qualified System.FilePath as FilePath
import qualified Text.XML as Xml

application :: Context.Context -> Wai.Application
application context request respond = do
    let
        method = Convert.utf8ToString $ Wai.requestMethod request
        path = fmap Convert.textToString $ Wai.pathInfo request
        config = Context.config context
        dataDirectory = Config.dataDirectory config
        baseUrl = Config.baseUrl config
        clientId = Config.clientId config
    case (method, path) of
        ("GET", []) -> respond . Response.xml Http.ok200 [] $ Xml.Document
            (Xml.Prologue
                [Xml.MiscInstruction $ Xml.Instruction
                    (Convert.stringToText "xml-stylesheet")
                    (Convert.stringToText $ "type=\"text/xsl\" charset=\"UTF-8\" href=\"" <> Xml.escape baseUrl <> "/monadoc.xsl\"")]
                Nothing
                [])
            (Xml.element "monadoc" [] [])
            []
        ("GET", ["bootstrap.css"]) -> do
            contents <- LazyByteString.readFile $ FilePath.combine dataDirectory "bootstrap.css"
            respond $ Response.lazyByteString Http.ok200 [(Http.hContentType, Convert.stringToUtf8 "text/css; charset=UTF-8")] contents
        ("GET", ["favicon.ico"]) -> respond $ Response.status Http.found302
            [ (Http.hLocation, Convert.stringToUtf8 "monadoc.svg")
            ]
        ("GET", ["monadoc.svg"]) -> do
            contents <- LazyByteString.readFile $ FilePath.combine dataDirectory "monadoc.svg"
            respond $ Response.lazyByteString Http.ok200 [(Http.hContentType, Convert.stringToUtf8 "image/svg+xml; charset=UTF-8")] contents
        ("GET", ["monadoc.xsl"]) -> respond . Response.xml Http.ok200 [] $ Xml.Document
            (Xml.Prologue [] Nothing [])
            (Xml.element "xsl:stylesheet" [("version", "1.0")]
                [ Xml.node "xsl:output"
                    [ ("method", "html")
                    , ("media-type", "text/html")
                    , ("encoding", "UTF-8")
                    -- https://stackoverflow.com/a/3404922/1274282
                    , ("doctype-system", "about:legacy-compat")
                    ] []
                , Xml.node "xsl:variable" [("name", "base-url")] [Xml.content baseUrl]
                , Xml.node "xsl:template" [("match", "/")]
                    [ Xml.node "html" [("lang", "en-US")]
                        [ Xml.node "head" []
                            [ Xml.node "meta"
                                [ ("name", "viewport")
                                , ("content", "initial-scale = 1, width = device-width")
                                ] []
                            , Xml.node "title" [] [Xml.content "Monadoc"]
                            , Xml.node "link"
                                [ ("rel", "stylesheet")
                                , ("href", "{$base-url}/bootstrap.css")
                                ] []
                            , Xml.node "link"
                                [ ("rel", "icon")
                                , ("type", "image/svg+xml")
                                , ("href", "{$base-url}/monadoc.svg")
                                ]
                                []
                            ]
                        , Xml.node "body" []
                            [ Xml.node "header" [("class", "mb-3")]
                                [ Xml.node "nav" [("class", "navbar navbar-light bg-light")]
                                    [ Xml.node "div" [("class", "container-fluid")]
                                        [ Xml.node "a" [("class", "navbar-brand"), ("href", "{$base-url}/")] [Xml.content "Monadoc"]
                                        , Xml.node "ul" [("class", "navbar-nav")]
                                            [ Xml.node "li" [("class", "nav-item")]
                                                [ Xml.node "a"
                                                    [ ("class", "nav-link")
                                                    , ("href", "https://github.com/login/oauth/authorize?client_id=" <> clientId <> "&redirect_uri={$base-url}/TODO")
                                                    ] [Xml.content "Log in"]
                                                ]
                                            ]
                                        ]
                                    ]
                                ]
                            , Xml.node "main" [("class", "container-fluid mt-3 mb-3")]
                                [ Xml.node "p" [] [Xml.content "\x1f516 Better Haskell documentation."]
                                ]
                            , Xml.node "footer" [("class", "container-fluid pt-3 mt-3 text-muted border-top")]
                                [ Xml.node "p" []
                                    [ Xml.content "Powered by "
                                    , Xml.node "a" [("href", "https://github.com/tfausak/monadoc")] [Xml.content "Monadoc"]
                                    , Xml.content $ " version " <> Convert.versionToString Package.version <> "."
                                    ]
                                ]
                            ]
                        ]
                    ]
                ])
            []
        ("GET", ["robots.txt"]) -> respond . Response.string Http.ok200 [] $ unlines ["User-Agent: *", "Allow: /"]
        _ -> respond $ Response.status Http.notFound404 []
