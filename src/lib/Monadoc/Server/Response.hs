module Monadoc.Server.Response where

import qualified Data.ByteString.Lazy as LazyByteString
import qualified Monadoc.Utility.Convert as Convert
import qualified Monadoc.Utility.Xml as Xml
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Text.XML as Xml

file :: Http.Status -> Http.ResponseHeaders -> FilePath -> IO Wai.Response
file s h = fmap (lazyByteString s h) . LazyByteString.readFile

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
        (Xml.element "status" []
            [ Xml.node "code" []
                [Xml.content . show $ Http.statusCode s]
            , Xml.node "message" []
                [Xml.content . Convert.utf8ToString $ Http.statusMessage s]
            ])
        []

string :: Http.Status -> Http.ResponseHeaders -> String -> Wai.Response
string s h =
    lazyByteString s ((Http.hContentType, Convert.stringToUtf8 "text/plain; charset=UTF-8") : h)
    . LazyByteString.fromStrict
    . Convert.stringToUtf8

xml :: Http.Status -> Http.ResponseHeaders -> Xml.Document -> Wai.Response
xml s h d = lazyByteString
    s
    ((Http.hContentType, Convert.stringToUtf8 "text/xml; charset=UTF-8") : h)
    $ Xml.renderLBS Xml.def { Xml.rsPretty = True } d
