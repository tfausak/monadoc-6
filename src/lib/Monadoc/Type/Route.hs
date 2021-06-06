module Monadoc.Type.Route where

import Monadoc.Prelude

import qualified Data.List as List
import qualified Monadoc.Class.ToXml as ToXml
import qualified Monadoc.Type.PackageName as PackageName
import qualified Monadoc.Type.Revision as Revision
import qualified Monadoc.Type.Version as Version

data Route
    = Account
    | Bootstrap
    | Callback
    | Favicon
    | Index
    | LogOut
    | Logo
    | Package PackageName.PackageName
    | Revision PackageName.PackageName Version.Version Revision.Revision
    | Revoke
    | Robots
    | Search
    | Template
    | Version PackageName.PackageName Version.Version
    deriving (Eq, Show)

instance ToXml.ToXml Route where
    toXml = ToXml.toXml . toString

fromStrings :: [String] -> Maybe Route
fromStrings path = case path of
    [] -> Just Index
    ["static", "bootstrap.css"] -> Just Bootstrap
    ["favicon.ico"] -> Just Favicon
    ["account", "callback"] -> Just Callback
    ["static", "monadoc.svg"] -> Just Logo
    ["static", "monadoc.xsl"] -> Just Template
    ["robots.txt"] -> Just Robots
    ["package", rawPackageName] -> Package
        <$> hush (tryInto @PackageName.PackageName rawPackageName)
    ["package", rawPackageName, rawVersion] -> Version
        <$> hush (tryInto @PackageName.PackageName rawPackageName)
        <*> hush (tryInto @Version.Version rawVersion)
    ["package", rawPackageName, rawVersion, rawRevision] -> Revision
        <$> hush (tryInto @PackageName.PackageName rawPackageName)
        <*> hush (tryInto @Version.Version rawVersion)
        <*> hush (tryInto @Revision.Revision rawRevision)
    ["search"] -> Just Search
    ["account"] -> Just Account
    ["account", "log-out"] -> Just LogOut
    ["account", "revoke"] -> Just Revoke
    _ -> Nothing

toString :: Route -> String
toString = cons '/' . List.intercalate "/" . toStrings

toStrings :: Route -> [String]
toStrings route = case route of
    Account -> ["account"]
    Bootstrap -> ["static", "bootstrap.css"]
    Callback -> ["account", "callback"]
    Favicon -> ["favicon.ico"]
    Index -> []
    LogOut -> ["account", "log-out"]
    Logo -> ["static", "monadoc.svg"]
    Package packageName -> ["package", into @String packageName]
    Revision packageName version revision -> ["package", into @String packageName, into @String version, into @String revision]
    Revoke -> ["account", "revoke"]
    Robots -> ["robots.txt"]
    Search -> ["search"]
    Template -> ["static", "monadoc.xsl"]
    Version packageName version -> ["package", into @String packageName, into @String version]
