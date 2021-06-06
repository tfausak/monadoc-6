module Monadoc.Model.Package where

import Monadoc.Prelude

import qualified Data.Maybe as Maybe
import qualified Data.Time as Time
import qualified Database.SQLite.Simple as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import qualified Monadoc.Type.Migration as Migration
import qualified Monadoc.Type.PackageName as PackageName
import qualified Monadoc.Type.Revision as Revision
import qualified Monadoc.Type.Sha256 as Sha256
import qualified Monadoc.Type.Version as Version

-- TODO: Add more fields to this model.
data Package = Package
    { contents :: ByteString
    , hash :: Sha256.Sha256
    , name :: PackageName.PackageName
    , revision :: Revision.Revision
    , uploadedAt :: Time.UTCTime
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

instance Sql.ToRow Package where
    toRow package =
        [ Sql.toField $ contents package
        , Sql.toField $ hash package
        , Sql.toField $ name package
        , Sql.toField $ revision package
        , Sql.toField $ uploadedAt package
        , Sql.toField $ version package
        ]

migrations :: [Migration.Migration]
migrations =
    [ Migration.new 2021 6 5 8 11 0
        "create table package \
        \(contents blob not null, \
        \hash text not null, \
        \name text not null, \
        \revision integer not null, \
        \uploadedAt text not null, \
        \version text not null, \
        \unique (name, version, revision))"
    , Migration.new 2021 6 5 17 13 0
        "create index package_uploadedAt on package (uploadedAt)"
    ]

insertOrUpdate :: Sql.Connection -> Package -> IO ()
insertOrUpdate c = Sql.execute c $ into @Sql.Query
    "insert into package (contents, hash, name, revision, uploadedAt, version) \
    \values (?, ?, ?, ?, ?, ?) \
    \on conflict (name, version, revision) \
    \do update set contents = excluded.contents, \
    \hash = excluded.hash, \
    \uploadedAt = excluded.uploadedAt"

select
    :: Sql.Connection
    -> PackageName.PackageName
    -> Version.Version
    -> Revision.Revision
    -> IO (Maybe Package)
select c n v r = fmap Maybe.listToMaybe $ Sql.query c (into @Sql.Query
    "select contents, hash, name, revision, uploadedAt, version \
    \from package \
    \where name = ? \
    \and version = ? \
    \and revision = ?") (n, v, r)

selectByName :: Sql.Connection -> PackageName.PackageName -> IO [Package]
selectByName c n = Sql.query c (into @Sql.Query
    "select contents, hash, name, revision, uploadedAt, version \
    \from package \
    \where name = ?") [n]

selectByNameAndVersion
    :: Sql.Connection
    -> PackageName.PackageName
    -> Version.Version
    -> IO [Package]
selectByNameAndVersion c n v = Sql.query c (into @Sql.Query
    "select contents, hash, name, revision, uploadedAt, version \
    \from package \
    \where name = ? \
    \and version = ?") (n, v)

selectRecent :: Sql.Connection -> IO [Package]
selectRecent c = Sql.query_ c $ into @Sql.Query
    "select contents, hash, name, revision, uploadedAt, version \
    \from package \
    \order by uploadedAt desc \
    \limit 10"

selectByNameLike :: Sql.Connection -> String -> IO [PackageName.PackageName]
selectByNameLike c x = fmap (fmap Sql.fromOnly) $ Sql.query c (into @Sql.Query
    "select distinct name \
    \from package \
    \where name like ? escape ? \
    \order by uploadedAt desc \
    \limit 10") ("%" <> escapeLike x <> "%", "\\")

escapeLike :: String -> String
escapeLike = foldMap $ \ c -> case c of
    '%' -> "\\%"
    '_' -> "\\_"
    '\\' -> "\\\\"
    _ -> [c]
