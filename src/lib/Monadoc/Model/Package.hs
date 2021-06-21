module Monadoc.Model.Package where

import Monadoc.Prelude

import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Time as Time
import qualified Monadoc.Vendor.Sql as Sql
import qualified Monadoc.Model.HackageUser as HackageUser
import qualified Monadoc.Model.Migration as Migration
import qualified Monadoc.Type.BuildType as BuildType
import qualified Monadoc.Type.CabalVersion as CabalVersion
import qualified Monadoc.Type.Key as Key
import qualified Monadoc.Type.License as License
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Type.PackageName as PackageName
import qualified Monadoc.Type.Revision as Revision
import qualified Monadoc.Type.Sha256 as Sha256
import qualified Monadoc.Type.Version as Version

type Model = Model.Model Package

type Key = Key.Key Package

data Package = Package
    { author :: Text
    -- ^ This is a free form text field. It is usually empty. Often it matches
    -- one of these patterns:
    --
    -- - Jane Doe
    -- - Jane Doe <jane@doe.example>
    -- - jane@doe.example
    -- - Jane Doe, John Smith
    --     - Or separated by "and" or "&".
    -- - Jane Doe and John Smith
    -- - JaneDoe
    --     - Presumably a Hackage user name.
    -- - AUTHORS
    --     - Presumably a file name that's included in the package tarball.
    -- - Acme Software LLC
    , bugReports :: Text
    -- ^ This is a free form text field. It is usually empty. Often it matches
    -- one of these patterns:
    --
    -- - https://github.com/jane-doe/some-package/issues
    -- - mailto:jane@doe.example
    , buildType :: BuildType.BuildType
    , cabalVersion :: CabalVersion.CabalVersion
    , category :: Text
    -- ^ This is a free form text field. Somewhat surprisingly, it is not
    -- usually empty. It's almost always a comma separated list of categories.
    -- There is not fixed set of categories to pick from.
    , copyright :: Text
    -- ^ This is a free form text field. It is usually empty. Often it looks
    -- like the author field, but with some additional data like years. For
    -- example:
    --
    -- - Jane Doe
    -- - Jane Doe <jane@doe.example>
    -- - Copyright (c) 1970 Jane Doe
    -- - (c) 1970 Jane Doe
    -- - 1970 Jane Doe
    -- - 1970-2021 Jane Doe
    , description :: Text
    , hash :: Sha256.Sha256
    , homepage :: Text
    -- ^ This is a free form text field. It is usually empty. Often it matches
    -- one of these patterns:
    --
    -- - https://github.com/jane-doe/some-package
    -- - https://some-package.github.io
    -- - https://some-package.example
    , license :: License.License
    , maintainer :: Text
    -- ^ This is a free form text field. It's very similar to the author field,
    -- except it is usually not empty.
    , name :: PackageName.PackageName
    , pkgUrl :: Text
    -- ^ This is a free form text field. It's almost always empty. It's similar
    -- to the bug reports and homepage fields, but people seem to use it for
    -- source code.
    , revision :: Revision.Revision
    , stability :: Text
    -- ^ This is a free form text field. It's usually empty. There's no fixed
    -- set of stabilities, but people often pick "experimental" or "stable".
    , synopsis :: Text
    , uploadedAt :: Time.UTCTime
    , uploadedBy :: HackageUser.Key
    , version :: Version.Version
    } deriving (Eq, Show)

instance Sql.FromRow Package where
    fromRow = Package
        <$> Sql.field
        <*> Sql.field
        <*> Sql.field
        <*> Sql.field
        <*> Sql.field
        <*> Sql.field
        <*> Sql.field
        <*> Sql.field
        <*> Sql.field
        <*> Sql.field
        <*> Sql.field
        <*> Sql.field
        <*> Sql.field
        <*> Sql.field
        <*> Sql.field
        <*> Sql.field
        <*> Sql.field
        <*> Sql.field
        <*> Sql.field

instance Sql.ToRow Package where
    toRow package =
        [ Sql.toField $ author package
        , Sql.toField $ bugReports package
        , Sql.toField $ buildType package
        , Sql.toField $ cabalVersion package
        , Sql.toField $ category package
        , Sql.toField $ copyright package
        , Sql.toField $ description package
        , Sql.toField $ hash package
        , Sql.toField $ homepage package
        , Sql.toField $ license package
        , Sql.toField $ maintainer package
        , Sql.toField $ name package
        , Sql.toField $ pkgUrl package
        , Sql.toField $ revision package
        , Sql.toField $ stability package
        , Sql.toField $ synopsis package
        , Sql.toField $ uploadedAt package
        , Sql.toField $ uploadedBy package
        , Sql.toField $ version package
        ]

migrations :: [Migration.Migration]
migrations =
    [ Migration.new 2021 6 5 8 11 0
        "create table package \
        \(key integer not null primary key, \
        \author text not null, \
        \bugReports text not null, \
        \buildType text not null, \
        \cabalVersion text not null, \
        \category text not null, \
        \copyright text not null, \
        \description text not null, \
        \hash text not null, \
        \homepage text not null, \
        \license text not null, \
        \maintainer text not null, \
        \name text not null, \
        \pkgUrl text not null, \
        \revision integer not null, \
        \stability text not null, \
        \synopsis text not null, \
        \uploadedAt text not null, \
        \uploadedBy integer not null, \
        \version text not null, \
        \unique (name, version, revision))"
    , Migration.new 2021 6 5 17 13 0
        "create index package_uploadedAt on package (uploadedAt)"
    , Migration.new 2021 6 8 22 20 0
        "create index package_name on package (name)"
    ]

insertOrUpdate :: Sql.Connection -> Package -> IO Key
insertOrUpdate connection package = do
    Sql.execute
        connection
            "insert into package (author, bugReports, buildType, cabalVersion, category, copyright, description, hash, homepage, license, maintainer, name, pkgUrl, revision, stability, synopsis, uploadedAt, uploadedBy, version) \
            \values (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?) \
            \on conflict (name, version, revision) \
            \do update set author = excluded.author, \
            \bugReports = excluded.bugReports, \
            \buildType = excluded.buildType, \
            \cabalVersion = excluded.cabalVersion, \
            \category = excluded.category, \
            \copyright = excluded.copyright, \
            \description = excluded.description, \
            \hash = excluded.hash, \
            \homepage = excluded.homepage, \
            \license = excluded.license, \
            \maintainer = excluded.maintainer, \
            \pkgUrl = excluded.pkgUrl, \
            \stability = excluded.stability, \
            \synopsis = excluded.synopsis, \
            \uploadedAt = excluded.uploadedAt, \
            \uploadedBy = excluded.uploadedBy"
        package
    [Sql.Only key] <- Sql.query connection
        "select key from package where name = ? and version = ? and revision = ?"
        (name package, version package, revision package)
    pure key

select
    :: Sql.Connection
    -> PackageName.PackageName
    -> Version.Version
    -> Revision.Revision
    -> IO (Maybe Model)
select c n v r = fmap Maybe.listToMaybe $ Sql.query c
    "select key, author, bugReports, buildType, cabalVersion, category, copyright, description, hash, homepage, license, maintainer, name, pkgUrl, revision, stability, synopsis, uploadedAt, uploadedBy, version \
    \from package \
    \where name = ? \
    \and version = ? \
    \and revision = ?" (n, v, r)

selectHashes
    :: Sql.Connection
    -> IO (Map (PackageName.PackageName, Version.Version, Revision.Revision) Sha256.Sha256)
selectHashes connection = do
    rows <- Sql.query_ connection "select name, version, revision, hash from package"
    pure $ foldr (\ (n, v, r, h) -> Map.insert (n, v, r) h) Map.empty rows

selectByName :: Sql.Connection -> PackageName.PackageName -> IO [Model]
selectByName c n = Sql.query c
    "select key, author, bugReports, buildType, cabalVersion, category, copyright, description, hash, homepage, license, maintainer, name, pkgUrl, revision, stability, synopsis, uploadedAt, uploadedBy, version \
    \from package \
    \where name = ?" [n]

selectByNameAndVersion
    :: Sql.Connection
    -> PackageName.PackageName
    -> Version.Version
    -> IO [Model]
selectByNameAndVersion c n v = Sql.query c
    "select key, author, bugReports, buildType, cabalVersion, category, copyright, description, hash, homepage, license, maintainer, name, pkgUrl, revision, stability, synopsis, uploadedAt, uploadedBy, version \
    \from package \
    \where name = ? \
    \and version = ?" (n, v)

selectRecent :: Sql.Connection -> IO [Model]
selectRecent c = Sql.query_ c
    "select key, author, bugReports, buildType, cabalVersion, category, copyright, description, hash, homepage, license, maintainer, name, pkgUrl, revision, stability, synopsis, uploadedAt, uploadedBy, version \
    \from package \
    \order by uploadedAt desc \
    \limit 32"

selectNamesLike :: Sql.Connection -> String -> IO [PackageName.PackageName]
selectNamesLike c x = fmap (fmap Sql.fromOnly) $ Sql.query c
    "select name \
    \from package \
    \where name like ? escape ? \
    \group by name \
    \order by uploadedAt desc \
    \limit 32" (x, "\\")

escapeLike :: String -> String
escapeLike = foldMap $ \ c -> case c of
    '%' -> "\\%"
    '_' -> "\\_"
    '\\' -> "\\\\"
    _ -> [c]

selectNamesAndVersions :: Sql.Connection -> IO [(PackageName.PackageName, Version.Version)]
selectNamesAndVersions connection = Sql.query_ connection
    "select name, version from package group by name, version"
