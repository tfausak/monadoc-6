module Monadoc.Server.Response where

import qualified Data.ByteString.Lazy as LazyByteString
import qualified Monadoc.Utility.Convert as Convert
import qualified Monadoc.Utility.Xml as Xml
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Text.XML as Xml

lazyByteString :: Http.Status -> Http.ResponseHeaders -> LazyByteString.ByteString -> Wai.Response
lazyByteString s h b = Wai.responseLBS
    s
    ((Http.hContentLength, Convert.stringToUtf8 . show $ LazyByteString.length b) : h)
    b

status :: Http.Status -> Http.ResponseHeaders -> Wai.Response
status s h = xml
    s
    h
    $ Xml.Document
        (Xml.Prologue [] Nothing [])
        (Xml.element (Xml.name "status") []
            [ Xml.node (Xml.name "code") []
                [Xml.content . show $ Http.statusCode s]
            , Xml.node (Xml.name "message") []
                [Xml.content . Convert.utf8ToString $ Http.statusMessage s]
            ])
        []

xml :: Http.Status -> Http.ResponseHeaders -> Xml.Document -> Wai.Response
xml s h d = lazyByteString
    s
    ((Http.hContentType, Convert.stringToUtf8 "text/xml; charset=UTF-8") : h)
    $ Xml.renderLBS Xml.def { Xml.rsPretty = True } d
