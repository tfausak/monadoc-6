module Monadoc.Utility.Xml where

import Monadoc.Prelude

import qualified Data.Bifunctor as Bifunctor
import qualified Data.Map as Map
import qualified Monadoc.Utility.Convert as Convert
import qualified Text.XML as Xml

escape :: String -> String
escape = foldMap $ \ c -> case c of
    '"' -> "&quot;"
    '\'' -> "&apos;"
    '&' -> "&amp;"
    '<' -> "&lt;"
    '>' -> "&gt;"
    _ -> [c]

name :: String -> Xml.Name
name s = case break (== ':') s of
    (prefix, ':' : local)
        | prefix == "xsl" -> Xml.Name
            (Convert.stringToText local)
            (Just $ Convert.stringToText "http://www.w3.org/1999/XSL/Transform")
            (Just $ Convert.stringToText prefix)
    _ -> Xml.Name (Convert.stringToText s) Nothing Nothing

node :: String -> [(String, String)] -> [Xml.Node] -> Xml.Node
node n as = Xml.NodeElement . element n as

element :: String -> [(String, String)] -> [Xml.Node] -> Xml.Element
element n = Xml.Element (name n)
    . Map.fromList
    . fmap (Bifunctor.bimap name Convert.stringToText)

content :: String -> Xml.Node
content = Xml.NodeContent . Convert.stringToText
