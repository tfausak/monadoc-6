{-# LANGUAGE TypeApplications #-}

module Monadoc.Type.Route where

import qualified Data.ByteString as ByteString
import qualified Data.List as List
import qualified Monadoc.Class.ToXml as ToXml
import qualified Monadoc.Type.ComponentId as ComponentId
import qualified Monadoc.Type.ModuleName as ModuleName
import qualified Monadoc.Type.PackageName as PackageName
import qualified Monadoc.Type.Release as Release
import qualified Monadoc.Utility.Either as Either
import qualified Network.HTTP.Types as Http
import qualified Witch

data Route
    = Account
    | AppleTouchIcon
    | Bootstrap
    | Callback
    | Component
        PackageName.PackageName
        Release.Release
        ComponentId.ComponentId
    | Favicon
    | File
        PackageName.PackageName
        Release.Release
        FilePath
    | HealthCheck
    | Index
    | LogOut
    | Logo
    | Module
        PackageName.PackageName
        Release.Release
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
        <$> Either.toMaybe (Witch.tryInto @PackageName.PackageName p)
    ["search"] -> Search <$> getQuery query
    ["account"] -> Just Account
    ["account", "log-out"] -> Just LogOut
    ["account", "revoke"] -> Just Revoke
    ["package", p, "version", r, "component", c] -> Component
        <$> Either.toMaybe (Witch.tryInto @PackageName.PackageName p)
        <*> Either.toMaybe (Witch.tryInto @Release.Release r)
        <*> Either.toMaybe (Witch.tryInto @ComponentId.ComponentId c)
    ["health-check"] -> Just HealthCheck
    ["apple-touch-icon.png"] -> Just AppleTouchIcon
    ["package", p, "version", r, "file"] -> File
        <$> Either.toMaybe (Witch.tryInto @PackageName.PackageName p)
        <*> Either.toMaybe (Witch.tryInto @Release.Release r)
        <*> getPath query
    ["package", p, "version", r, "component", c, "module", m] -> Module
        <$> Either.toMaybe (Witch.tryInto @PackageName.PackageName p)
        <*> Either.toMaybe (Witch.tryInto @Release.Release r)
        <*> Either.toMaybe (Witch.tryInto @ComponentId.ComponentId c)
        <*> Either.toMaybe (Witch.tryInto @ModuleName.ModuleName m)
    ["package", p, "version", r] -> Release
        <$> Either.toMaybe (Witch.tryInto @PackageName.PackageName p)
        <*> Either.toMaybe (Witch.tryInto @Release.Release r)
    _ -> Nothing

getPath :: Http.Query -> Maybe FilePath
getPath query = do
    maybeByteString <- lookup (Witch.into @ByteString.ByteString "path") query
    byteString <- maybeByteString
    Either.toMaybe $ Witch.tryInto @String byteString

getQuery :: Http.Query -> Maybe (Maybe String)
getQuery query = case lookup (Witch.into @ByteString.ByteString "query") query of
    Just (Just x) -> case Witch.tryInto @String x of
        Left _ -> Nothing
        Right y -> Just $ Just y
    _ -> Just Nothing

toString :: Route -> String
toString route =
    let (path, query) = render route
    in '/' : List.intercalate "/" path <> Witch.unsafeInto @String (Http.renderQuery True query)

render :: Route -> ([String], Http.Query)
render route = case route of
    Account -> (["account"], [])
    AppleTouchIcon -> (["apple-touch-icon.png"], [])
    Bootstrap -> (["static", "bootstrap.css"], [])
    Callback -> (["account", "callback"], [])
    Component p r c -> (["package", Witch.into @String p, "version", Witch.into @String r, "component", Witch.into @String c], [])
    Favicon -> (["favicon.ico"], [])
    File p r path -> (["package", Witch.into @String p, "version", Witch.into @String r, "file"], [(Witch.into @ByteString.ByteString "path", Just $ Witch.into @ByteString.ByteString path)])
    HealthCheck -> (["health-check"], [])
    Index -> ([], [])
    LogOut -> (["account", "log-out"], [])
    Logo -> (["static", "monadoc.svg"], [])
    Module p r c m -> (["package", Witch.into @String p, "version", Witch.into @String r, "component", Witch.into @String c, "module", Witch.into @String m], [])
    Package p -> (["package", Witch.into @String p], [])
    Release p r -> (["package", Witch.into @String p, "version", Witch.into @String r], [])
    Revoke -> (["account", "revoke"], [])
    Robots -> (["robots.txt"], [])
    Search maybeQuery -> (["search"], maybe [] (\ query -> [(Witch.into @ByteString.ByteString "query", Just $ Witch.into @ByteString.ByteString query)]) maybeQuery)
    Template -> (["static", "monadoc.xsl"], [])
