module Monadoc.Utility.Xml where

import Monadoc.Prelude

import qualified Data.Bifunctor as Bifunctor
import qualified Data.Map as Map
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
            (into @Text local)
            (Just $ into @Text "http://www.w3.org/1999/XSL/Transform")
            (Just $ into @Text prefix)
    _ -> Xml.Name (into @Text s) Nothing Nothing

node :: String -> [(String, String)] -> [Xml.Node] -> Xml.Node
node n xs = Xml.NodeElement . element n xs

element :: String -> [(String, String)] -> [Xml.Node] -> Xml.Element
element n = Xml.Element (name n)
    . Map.fromList
    . fmap (Bifunctor.bimap name (into @Text))
