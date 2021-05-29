module Monadoc.Server.Response where

import Monadoc.Prelude

import qualified Data.ByteString.Lazy as LazyByteString
import qualified Monadoc.Utility.Xml as Xml
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Text.XML as Xml

file :: Http.Status -> Http.ResponseHeaders -> FilePath -> IO Wai.Response
file s h = fmap (lazyByteString s h) <. LazyByteString.readFile

lazyByteString :: Http.Status -> Http.ResponseHeaders -> LazyByteString -> Wai.Response
lazyByteString s h b = Wai.responseLBS
    s
    ((Http.hContentLength, into @ByteString <. show <| LazyByteString.length b) : h)
    b

status :: Http.Status -> Http.ResponseHeaders -> Wai.Response
status s h = xml
    s
    h
    <| Xml.Document
        (Xml.Prologue [] Nothing [])
        (Xml.element "status" []
            [ Xml.node "code" []
                [Xml.content <. show <| Http.statusCode s]
            , Xml.node "message" []
                [Xml.content <. unsafeInto @String <| Http.statusMessage s]
            ])
        []

string :: Http.Status -> Http.ResponseHeaders -> String -> Wai.Response
string s h =
    lazyByteString s ((Http.hContentType, into @ByteString "text/plain; charset=UTF-8") : h)
    <. into @LazyByteString
    <. into @ByteString

xml :: Http.Status -> Http.ResponseHeaders -> Xml.Document -> Wai.Response
xml s h d = lazyByteString
    s
    ((Http.hContentType, into @ByteString "text/xml; charset=UTF-8") : h)
    <| Xml.renderLBS Xml.def { Xml.rsPretty = True } d
