module Monadoc.Server.Response where

import Monadoc.Prelude

import qualified Data.ByteString as ByteString
import qualified Monadoc.Class.ToXml as ToXml
import qualified Monadoc.Utility.Xml as Xml
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Text.XML as Xml

byteString :: Http.Status -> Http.ResponseHeaders -> ByteString -> Wai.Response
byteString s h b = Wai.responseLBS
    s
    ((Http.hContentLength, into @ByteString . show $ ByteString.length b) : h)
    (into @LazyByteString b)

file :: Http.Status -> Http.ResponseHeaders -> FilePath -> IO Wai.Response
file s h = fmap (byteString s h) . ByteString.readFile

lazyByteString :: Http.Status -> Http.ResponseHeaders -> LazyByteString -> Wai.Response
lazyByteString s h = byteString s h . into @ByteString

status :: Http.Status -> Http.ResponseHeaders -> Wai.Response
status s h = xml
    s
    h
    $ Xml.Document
        (Xml.Prologue [] Nothing [])
        (Xml.element "status" []
            [ Xml.node "code" []
                [ToXml.toXml $ Http.statusCode s]
            , Xml.node "message" []
                [ToXml.toXml . unsafeInto @String $ Http.statusMessage s]
            ])
        []

string :: Http.Status -> Http.ResponseHeaders -> String -> Wai.Response
string s h =
    byteString s ((Http.hContentType, into @ByteString "text/plain; charset=UTF-8") : h)
    . into @ByteString

xml :: Http.Status -> Http.ResponseHeaders -> Xml.Document -> Wai.Response
xml s h d = lazyByteString
    s
    ((Http.hContentType, into @ByteString "text/xml; charset=UTF-8") : h)
    $ Xml.renderLBS Xml.def d
