module Monadoc.Type.Route where

import Monadoc.Prelude

import qualified Data.List as List
import qualified Monadoc.Class.ToXml as ToXml
import qualified Monadoc.Type.ComponentId as ComponentId
import qualified Monadoc.Type.ModuleName as ModuleName
import qualified Monadoc.Type.PackageName as PackageName
import qualified Monadoc.Type.Release as Release
import qualified Monadoc.Type.Revision as Revision
import qualified Monadoc.Type.Version as Version
import qualified Network.HTTP.Types as Http

data Route
    = Account
    | AppleTouchIcon
    | Bootstrap
    | Callback
    | Component
        PackageName.PackageName
        Version.Version
        Revision.Revision
        ComponentId.ComponentId
    | Favicon
    | File
        PackageName.PackageName
        Version.Version
        FilePath
    | HealthCheck
    | Index
    | LogOut
    | Logo
    | Module
        PackageName.PackageName
        Version.Version
        Revision.Revision
        ComponentId.ComponentId
        ModuleName.ModuleName
    | Package
        PackageName.PackageName
    | Release
        PackageName.PackageName
        Release.Release
    | Revoke
    | Robots
    | Search
        (Maybe String)
    | Template
    | Version
        PackageName.PackageName
        Version.Version
    deriving (Eq, Show)

instance ToXml.ToXml Route where
    toXml = ToXml.toXml . toString

parse :: [String] -> Http.Query -> Maybe Route
parse path query = case path of
    [] -> Just Index
    ["static", "bootstrap.css"] -> Just Bootstrap
    ["favicon.ico"] -> Just Favicon
    ["account", "callback"] -> Just Callback
    ["static", "monadoc.svg"] -> Just Logo
    ["static", "monadoc.xsl"] -> Just Template
    ["robots.txt"] -> Just Robots
    ["package", p] -> Package
        <$> hush (tryInto @PackageName.PackageName p)
    ["package", p, "version", v] -> Version
        <$> hush (tryInto @PackageName.PackageName p)
        <*> hush (tryInto @Version.Version v)
    ["search"] -> Search <$> getQuery query
    ["account"] -> Just Account
    ["account", "log-out"] -> Just LogOut
    ["account", "revoke"] -> Just Revoke
    ["package", p, "version", v, "revision", r, "component", c] -> Component
        <$> hush (tryInto @PackageName.PackageName p)
        <*> hush (tryInto @Version.Version v)
        <*> hush (tryInto @Revision.Revision r)
        <*> hush (tryInto @ComponentId.ComponentId c)
    ["health-check"] -> Just HealthCheck
    ["apple-touch-icon.png"] -> Just AppleTouchIcon
    ["package", p, "version", v, "file"] -> File
        <$> hush (tryInto @PackageName.PackageName p)
        <*> hush (tryInto @Version.Version v)
        <*> getPath query
    ["package", p, "version", v, "revision", r, "component", c, "module", m] -> Module
        <$> hush (tryInto @PackageName.PackageName p)
        <*> hush (tryInto @Version.Version v)
        <*> hush (tryInto @Revision.Revision r)
        <*> hush (tryInto @ComponentId.ComponentId c)
        <*> hush (tryInto @ModuleName.ModuleName m)
    ["package", p, "release", r] -> Release
        <$> hush (tryInto @PackageName.PackageName p)
        <*> hush (tryInto @Release.Release r)
    _ -> Nothing

getPath :: Http.Query -> Maybe FilePath
getPath query = do
    maybeByteString <- lookup (into @ByteString "path") query
    byteString <- maybeByteString
    hush $ tryInto @String byteString

getQuery :: Http.Query -> Maybe (Maybe String)
getQuery query = case lookup (into @ByteString "query") query of
    Just (Just x) -> case tryInto @String x of
        Left _ -> Nothing
        Right y -> Just $ Just y
    _ -> Just Nothing

toString :: Route -> String
toString route =
    let (path, query) = render route
    in cons '/' (List.intercalate "/" path) <> unsafeInto @String (Http.renderQuery True query)

render :: Route -> ([String], Http.Query)
render route = case route of
    Account -> (["account"], [])
    AppleTouchIcon -> (["apple-touch-icon.png"], [])
    Bootstrap -> (["static", "bootstrap.css"], [])
    Callback -> (["account", "callback"], [])
    Component p v r c -> (["package", into @String p, "version", into @String v, "revision", into @String r, "component", into @String c], [])
    Favicon -> (["favicon.ico"], [])
    File p v path -> (["package", into @String p, "version", into @String v, "file"], [(into @ByteString "path", Just $ into @ByteString path)])
    HealthCheck -> (["health-check"], [])
    Index -> ([], [])
    LogOut -> (["account", "log-out"], [])
    Logo -> (["static", "monadoc.svg"], [])
    Module p v r c m -> (["package", into @String p, "version", into @String v, "revision", into @String r, "component", into @String c, "module", into @String m], [])
    Package p -> (["package", into @String p], [])
    Release p r -> (["package", into @String p, "release", into @String r], [])
    Revoke -> (["account", "revoke"], [])
    Robots -> (["robots.txt"], [])
    Search maybeQuery -> (["search"], maybe [] (\ query -> [(into @ByteString "query", Just $ into @ByteString query)]) maybeQuery)
    Template -> (["static", "monadoc.xsl"], [])
    Version p v -> (["package", into @String p, "version", into @String v], [])
