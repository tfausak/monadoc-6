module Monadoc.Server.Response where

import qualified Data.Map as Map
import qualified Monadoc.Utility.Convert as Convert
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Text.XML as Xml

status :: Http.Status -> Http.ResponseHeaders -> Wai.Response
status s h = xml
    s
    h
    $ Xml.Document
        (Xml.Prologue [] Nothing [])
        (Xml.Element
            (Xml.Name (Convert.stringToText "status") Nothing Nothing)
            Map.empty
            [ Xml.NodeElement $ Xml.Element
                (Xml.Name (Convert.stringToText "code") Nothing Nothing)
                Map.empty
                [Xml.NodeContent . Convert.stringToText . show $ Http.statusCode s]
            , Xml.NodeElement $ Xml.Element
                (Xml.Name (Convert.stringToText "message") Nothing Nothing)
                Map.empty
                [Xml.NodeContent . Convert.utf8ToText $ Http.statusMessage s]
            ])
        []

xml :: Http.Status -> Http.ResponseHeaders -> Xml.Document -> Wai.Response
xml s h d = Wai.responseLBS
    s
    ((Http.hContentType, Convert.stringToUtf8 "text/xml; charset=UTF-8") : h)
    $ Xml.renderLBS Xml.def { Xml.rsPretty = True } d
