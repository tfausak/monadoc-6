module Monadoc.Server.Application where

import qualified Data.Map as Map
import qualified Monadoc.Server.Response as Response
import qualified Monadoc.Utility.Convert as Convert
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
            (Xml.Element
                (Xml.Name (Convert.stringToText "monadoc") Nothing Nothing)
                Map.empty
                [])
            []
        ("GET", ["monadoc.xsl"]) -> respond . Response.xml Http.ok200 [] $ Xml.Document
            (Xml.Prologue [] Nothing [])
            (Xml.Element
                (Xml.Name
                    (Convert.stringToText "stylesheet")
                    (Just $ Convert.stringToText "http://www.w3.org/1999/XSL/Transform")
                    (Just $ Convert.stringToText "xsl"))
                (Map.fromList
                    [ (Xml.Name (Convert.stringToText "version") Nothing Nothing, Convert.stringToText "1.0")
                    ])
                [ Xml.NodeElement $ Xml.Element
                    (Xml.Name
                        (Convert.stringToText "output")
                        (Just $ Convert.stringToText "http://www.w3.org/1999/XSL/Transform")
                        (Just $ Convert.stringToText "xsl"))
                    (Map.fromList
                        [ (Xml.Name (Convert.stringToText "method") Nothing Nothing, Convert.stringToText "html")
                        ])
                    []
                , Xml.NodeElement $ Xml.Element
                    (Xml.Name
                        (Convert.stringToText "template")
                        (Just $ Convert.stringToText "http://www.w3.org/1999/XSL/Transform")
                        (Just $ Convert.stringToText "xsl"))
                    (Map.fromList
                        [ (Xml.Name (Convert.stringToText "match") Nothing Nothing, Convert.stringToText "/")
                        ])
                    [ Xml.NodeElement $ Xml.Element
                        (Xml.Name (Convert.stringToText "html") Nothing Nothing)
                        Map.empty
                        [ Xml.NodeElement $ Xml.Element
                            (Xml.Name (Convert.stringToText "head") Nothing Nothing)
                            Map.empty
                            [ Xml.NodeElement $ Xml.Element
                                (Xml.Name (Convert.stringToText "title") Nothing Nothing)
                                Map.empty
                                [Xml.NodeContent $ Convert.stringToText "Monadoc"]
                            ]
                        , Xml.NodeElement $ Xml.Element
                            (Xml.Name (Convert.stringToText "body") Nothing Nothing)
                            Map.empty
                            [ Xml.NodeElement $ Xml.Element
                                (Xml.Name (Convert.stringToText "h1") Nothing Nothing)
                                Map.empty
                                [Xml.NodeContent $ Convert.stringToText "Monadoc"]
                            ]
                        ]
                    ]
                ])
            []
        _ -> respond $ Response.status Http.notFound404 []
