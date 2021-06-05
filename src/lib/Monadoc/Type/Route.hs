module Monadoc.Type.Route where

import Monadoc.Prelude

import qualified Data.List as List
import qualified Monadoc.Type.PackageName as PackageName

-- | Be careful making changes to this type! Everything on the Haskell side
-- will stay up to date, but the XSL template has hard-coded strings in it.
data Route
    = Bootstrap
    | Callback
    | Favicon
    | Index
    | Logo
    | Package PackageName.PackageName
    | Robots
    | Template
    deriving (Eq, Show)

fromStrings :: [String] -> Maybe Route
fromStrings path = case path of
    [] -> Just Index
    ["bootstrap.css"] -> Just Bootstrap
    ["favicon.ico"] -> Just Favicon
    ["oauth", "callback"] -> Just Callback
    ["monadoc.svg"] -> Just Logo
    ["monadoc.xsl"] -> Just Template
    ["robots.txt"] -> Just Robots
    ["package", rawPackageName] -> Package <$> hush (tryInto @PackageName.PackageName rawPackageName)
    _ -> Nothing

toString :: Route -> String
toString = cons '/' . List.intercalate "/" . toStrings

toStrings :: Route -> [String]
toStrings route = case route of
    Bootstrap -> ["bootstrap.css"]
    Callback -> ["oauth", "callback"]
    Favicon -> ["favicon.ico"]
    Index -> []
    Logo -> ["monadoc.svg"]
    Package packageName -> ["package", into @String packageName]
    Robots -> ["robots.txt"]
    Template -> ["monadoc.xsl"]
