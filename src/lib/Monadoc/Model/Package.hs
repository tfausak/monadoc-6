module Monadoc.Model.Package where

import Monadoc.Prelude

import qualified Data.Maybe as Maybe
import qualified Data.Time as Time
import qualified Database.SQLite.Simple as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import qualified Monadoc.Type.BuildType as BuildType
import qualified Monadoc.Type.CabalVersion as CabalVersion
import qualified Monadoc.Type.Key as Key
import qualified Monadoc.Type.License as License
import qualified Monadoc.Model.Migration as Migration
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Type.PackageName as PackageName
import qualified Monadoc.Type.Revision as Revision
import qualified Monadoc.Type.Sha256 as Sha256
import qualified Monadoc.Type.Version as Version

data Package = Package
    { author :: Text
    , bugReports :: Text
    , buildType :: BuildType.BuildType
    , cabalVersion :: CabalVersion.CabalVersion
    , category :: Text
    , contents :: ByteString
    , copyright :: Text
    , description :: Text
    , hash :: Sha256.Sha256
    , homepage :: Text
    , license :: License.License
    , maintainer :: Text
    , name :: PackageName.PackageName
    , pkgUrl :: Text
    , revision :: Revision.Revision
    , stability :: Text
    , synopsis :: Text
    , uploadedAt :: Time.UTCTime
    , uploadedBy :: Key.Key
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
        <*> Sql.field

instance Sql.ToRow Package where
    toRow package =
        [ Sql.toField $ author package
        , Sql.toField $ bugReports package
        , Sql.toField $ buildType package
        , Sql.toField $ cabalVersion package
        , Sql.toField $ category package
        , Sql.toField $ contents package
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
        \contents blob not null, \
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

insertOrUpdate :: Sql.Connection -> Package -> IO Key.Key
insertOrUpdate connection package = do
    Sql.execute
        connection
        (into @Sql.Query
            "insert into package (author, bugReports, buildType, cabalVersion, category, contents, copyright, description, hash, homepage, license, maintainer, name, pkgUrl, revision, stability, synopsis, uploadedAt, uploadedBy, version) \
            \values (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?) \
            \on conflict (name, version, revision) \
            \do update set author = excluded.author, \
            \bugReports = excluded.bugReports, \
            \buildType = excluded.buildType, \
            \cabalVersion = excluded.cabalVersion, \
            \category = excluded.category, \
            \contents = excluded.contents, \
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
            \uploadedBy = excluded.uploadedBy")
        package
    fmap (from @Int64) $ Sql.lastInsertRowId connection

select
    :: Sql.Connection
    -> PackageName.PackageName
    -> Version.Version
    -> Revision.Revision
    -> IO (Maybe (Model.Model Package))
select c n v r = fmap Maybe.listToMaybe $ Sql.query c (into @Sql.Query
    "select key, author, bugReports, buildType, cabalVersion, category, contents, copyright, description, hash, homepage, license, maintainer, name, pkgUrl, revision, stability, synopsis, uploadedAt, uploadedBy, version \
    \from package \
    \where name = ? \
    \and version = ? \
    \and revision = ?") (n, v, r)

selectByName :: Sql.Connection -> PackageName.PackageName -> IO [Model.Model Package]
selectByName c n = Sql.query c (into @Sql.Query
    "select key, author, bugReports, buildType, cabalVersion, category, contents, copyright, description, hash, homepage, license, maintainer, name, pkgUrl, revision, stability, synopsis, uploadedAt, uploadedBy, version \
    \from package \
    \where name = ?") [n]

selectByNameAndVersion
    :: Sql.Connection
    -> PackageName.PackageName
    -> Version.Version
    -> IO [Model.Model Package]
selectByNameAndVersion c n v = Sql.query c (into @Sql.Query
    "select key, author, bugReports, buildType, cabalVersion, category, contents, copyright, description, hash, homepage, license, maintainer, name, pkgUrl, revision, stability, synopsis, uploadedAt, uploadedBy, version \
    \from package \
    \where name = ? \
    \and version = ?") (n, v)

selectRecent :: Sql.Connection -> IO [Model.Model Package]
selectRecent c = Sql.query_ c $ into @Sql.Query
    "select key, author, bugReports, buildType, cabalVersion, category, contents, copyright, description, hash, homepage, license, maintainer, name, pkgUrl, revision, stability, synopsis, uploadedAt, uploadedBy, version \
    \from package \
    \order by uploadedAt desc \
    \limit 10"

selectNamesLike :: Sql.Connection -> String -> IO [PackageName.PackageName]
selectNamesLike c x = fmap (fmap Sql.fromOnly) $ Sql.query c (into @Sql.Query
    "select distinct name \
    \from package \
    \where name like ? escape ? \
    \order by uploadedAt desc \
    \limit 10") (x, "\\")

escapeLike :: String -> String
escapeLike = foldMap $ \ c -> case c of
    '%' -> "\\%"
    '_' -> "\\_"
    '\\' -> "\\\\"
    _ -> [c]
