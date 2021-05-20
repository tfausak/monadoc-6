module Monadoc.Utility.Xml where

import qualified Data.Map as Map
import qualified Monadoc.Utility.Convert as Convert
import qualified Text.XML as Xml

name :: String -> Xml.Name
name s = case break (== ':') s of
    (prefix, ':' : local)
        | prefix == "xsl" -> Xml.Name
            (Convert.stringToText local)
            (Just $ Convert.stringToText "http://www.w3.org/1999/XSL/Transform")
            (Just $ Convert.stringToText prefix)
    _ -> Xml.Name (Convert.stringToText s) Nothing Nothing

node :: Xml.Name -> [(Xml.Name, String)] -> [Xml.Node] -> Xml.Node
node n as = Xml.NodeElement . element n as

element :: Xml.Name -> [(Xml.Name, String)] -> [Xml.Node] -> Xml.Element
element n = Xml.Element n . Map.fromList . fmap (fmap Convert.stringToText)

content :: String -> Xml.Node
content = Xml.NodeContent . Convert.stringToText
