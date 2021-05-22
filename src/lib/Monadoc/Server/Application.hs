module Monadoc.Server.Application where

import qualified Data.ByteString.Lazy as LazyByteString
import qualified Monadoc.Server.Response as Response
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Utility.Convert as Convert
import qualified Monadoc.Utility.Xml as Xml
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified System.FilePath as FilePath
import qualified Text.XML as Xml

application :: Context.Context -> Wai.Application
application context request respond = do
    let
        method = Convert.utf8ToString $ Wai.requestMethod request
        path = fmap Convert.textToString $ Wai.pathInfo request
    case (method, path) of
        ("GET", []) -> respond . Response.xml Http.ok200 [] $ Xml.Document
            (Xml.Prologue
                [Xml.MiscInstruction $ Xml.Instruction
                    (Convert.stringToText "xml-stylesheet")
                    (Convert.stringToText "type=\"text/xsl\" charset=\"UTF-8\" href=\"monadoc.xsl\"")]
                Nothing
                [])
            (Xml.element (Xml.name "monadoc") [] [])
            []
        ("GET", ["bootstrap.css"]) -> do
            contents <- LazyByteString.readFile $ FilePath.combine (Config.dataDirectory $ Context.config context) "bootstrap.css"
            respond $ Response.lazyByteString Http.ok200 [(Http.hContentType, Convert.stringToUtf8 "text/css; charset=UTF-8")] contents
        ("GET", ["favicon.ico"]) -> respond $ Response.status Http.found302
            [ (Http.hLocation, Convert.stringToUtf8 "monadoc.svg")
            ]
        ("GET", ["monadoc.svg"]) -> do
            contents <- LazyByteString.readFile $ FilePath.combine (Config.dataDirectory $ Context.config context) "monadoc.svg"
            respond $ Response.lazyByteString Http.ok200 [(Http.hContentType, Convert.stringToUtf8 "image/svg+xml; charset=UTF-8")] contents
        ("GET", ["monadoc.xsl"]) -> respond . Response.xml Http.ok200 [] $ Xml.Document
            (Xml.Prologue [] Nothing [])
            (Xml.element (Xml.name "xsl:stylesheet") [(Xml.name "version", "1.0")]
                [ Xml.node (Xml.name "xsl:output")
                    [ (Xml.name "method", "html")
                    -- https://stackoverflow.com/a/3404922/1274282
                    , (Xml.name "doctype-system", "about:legacy-compat")
                    ] []
                , Xml.node (Xml.name "xsl:template") [(Xml.name "match", "/")]
                    [ Xml.node (Xml.name "html") [(Xml.name "lang", "en-US")]
                        [ Xml.node (Xml.name "head") []
                            [ Xml.node (Xml.name "meta")
                                [ (Xml.name "name", "viewport")
                                , (Xml.name "content", "initial-scale = 1, width = device-width")
                                ] []
                            , Xml.node (Xml.name "title") [] [Xml.content "Monadoc"]
                            , Xml.node (Xml.name "link")
                                [ (Xml.name "rel", "stylesheet")
                                , (Xml.name "href", "bootstrap.css")
                                ] []
                            , Xml.node (Xml.name "link")
                                [ (Xml.name "rel", "icon")
                                , (Xml.name "type", "image/svg+xml")
                                , (Xml.name "href", "monadoc.svg")
                                ]
                                []
                            ]
                        , Xml.node (Xml.name "body") []
                            [ Xml.node (Xml.name "nav") [(Xml.name "class", "navbar navbar-light bg-light")]
                                [ Xml.node (Xml.name "div") [(Xml.name "class", "container-fluid")]
                                    [ Xml.node (Xml.name "a") [(Xml.name "class", "navbar-brand")] [Xml.content "Monadoc"]
                                    ]
                                ]
                            ]
                        ]
                    ]
                ])
            []
        _ -> respond $ Response.status Http.notFound404 []
