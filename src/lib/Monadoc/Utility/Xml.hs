module Monadoc.Utility.Xml where

import Monadoc.Prelude

import qualified Data.Bifunctor as Bifunctor
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Text.XML as Xml
import qualified Witch

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
            (Witch.into @Text.Text local)
            (Just $ Witch.into @Text.Text "http://www.w3.org/1999/XSL/Transform")
            (Just $ Witch.into @Text.Text prefix)
    _ -> Xml.Name (Witch.into @Text.Text s) Nothing Nothing

node :: String -> [(String, String)] -> [Xml.Node] -> Xml.Node
node n as = Xml.NodeElement . element n as

element :: String -> [(String, String)] -> [Xml.Node] -> Xml.Element
element n = Xml.Element (name n)
    . Map.fromList
    . fmap (Bifunctor.bimap name (Witch.into @Text.Text))

content :: String -> Xml.Node
content = Xml.NodeContent . Witch.into @Text.Text
