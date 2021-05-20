module Monadoc.Server.Application where

import qualified Monadoc.Server.Response as Response
import qualified Monadoc.Utility.Convert as Convert
import qualified Monadoc.Utility.Xml as Xml
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Text.XML as Xml

application :: Wai.Application
application request respond = do
    let
        method = Convert.utf8ToString $ Wai.requestMethod request
        path = fmap Convert.textToString $ Wai.pathInfo request
    case (method, path) of
        ("GET", []) -> respond . Response.xml Http.ok200 [] $ Xml.Document
            (Xml.Prologue
                [Xml.MiscInstruction $ Xml.Instruction
                    (Convert.stringToText "xml-stylesheet")
                    (Convert.stringToText "type=\"text/xsl\" href=\"monadoc.xsl\"")]
                Nothing
                [])
            (Xml.element (Xml.name "monadoc") [] [])
            []
        ("GET", ["favicon.ico"]) -> respond $ Response.status Http.found302
            [ (Http.hLocation, Convert.stringToUtf8 "monadoc.svg")
            ]
        ("GET", ["monadoc.svg"]) -> respond . Response.xml Http.ok200 [] $ Xml.Document
            (Xml.Prologue [] Nothing [])
            (Xml.element (Xml.name "svg")
                [ (Xml.name "xmlns", "http://www.w3.org/2000/svg")
                , (Xml.name "viewBox", "0 0 16 16")
                ]
                [ Xml.node (Xml.name "g")
                    [(Xml.name "fill", "#5e2ca5")]
                    [ Xml.node (Xml.name "path") [(Xml.name "d", "M 2 7 h 2 v 7 h -2 z")] []
                    , Xml.node (Xml.name "path") [(Xml.name "d", "M 12 7 h 2 v 7 h -2 z")] []
                    , Xml.node (Xml.name "path") [(Xml.name "d", "M 2 5 l 6 2 l 6 -2 v 2 l -6 2 l -6 -2 z")] []
                    , Xml.node (Xml.name "path") [(Xml.name "d", "M 2 2 l 6 2 l 6 -2 v 2 l -6 2 l -6 -2 z")] []
                    ]
                ])
            []
        ("GET", ["monadoc.xsl"]) -> respond . Response.xml Http.ok200 [] $ Xml.Document
            (Xml.Prologue [] Nothing [])
            (Xml.element (Xml.name "xsl:stylesheet") [(Xml.name "version", "1.0")]
                [ Xml.node (Xml.name "xsl:output") [(Xml.name "method", "html")] []
                , Xml.node (Xml.name "xsl:template") [(Xml.name "match", "/")]
                    [ Xml.node (Xml.name "html") []
                        [ Xml.node (Xml.name "head") []
                            [ Xml.node (Xml.name "title") [] [Xml.content "Monadoc"]
                            , Xml.node (Xml.name "link")
                                [ (Xml.name "rel", "icon")
                                , (Xml.name "type", "image/svg+xml")
                                , (Xml.name "href", "monadoc.svg")
                                ]
                                []
                            ]
                        , Xml.node (Xml.name "body") []
                            [ Xml.node (Xml.name "h1") [] [Xml.content "Monadoc"]
                            ]
                        ]
                    ]
                ])
            []
        _ -> respond $ Response.status Http.notFound404 []
