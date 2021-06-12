module Monadoc.Model.Session where

import Monadoc.Prelude

import qualified Data.Maybe as Maybe
import qualified Data.Time as Time
import qualified Database.SQLite.Simple as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import qualified Monadoc.Type.Guid as Guid
import qualified Monadoc.Type.Key as Key
import qualified Monadoc.Type.Migration as Migration
import qualified Monadoc.Type.Model as Model

data Session = Session
    { createdAt :: Time.UTCTime
    , deletedAt :: Maybe Time.UTCTime
    , guid :: Guid.Guid
    , updatedAt :: Time.UTCTime
    , userAgent :: String
    , userGithubId :: Int
    } deriving (Eq, Show)

instance Sql.FromRow Session where
    fromRow = Session
        <$> Sql.field
        <*> Sql.field
        <*> Sql.field
        <*> Sql.field
        <*> Sql.field
        <*> Sql.field

instance Sql.ToRow Session where
    toRow session =
        [ Sql.toField $ createdAt session
        , Sql.toField $ deletedAt session
        , Sql.toField $ guid session
        , Sql.toField $ updatedAt session
        , Sql.toField $ userAgent session
        , Sql.toField $ userGithubId session
        ]

migrations :: [Migration.Migration]
migrations =
    [ Migration.new 2021 5 23 18 22 0
        "create table session \
        \(key integer not null primary key, \
        \createdAt text not null, \
        \deletedAt text, \
        \guid text not null, \
        \updatedAt text not null, \
        \userAgent text not null, \
        \userGithubId integer not null)"
    , Migration.new 2021 6 12 9 24 0
        "create unique index session_guid on session (guid)"
    ]

insert :: Sql.Connection -> Session -> IO Key.Key
insert connection session = do
    Sql.execute
        connection
        (into @Sql.Query
            "insert into session \
            \(createdAt, deletedAt, guid, updatedAt, userAgent, userGithubId) \
            \values (?, ?, ?, ?, ?, ?)")
        session
    fmap (from @Int64) $ Sql.lastInsertRowId connection

selectByGuid :: Sql.Connection -> Guid.Guid -> IO (Maybe (Model.Model Session))
selectByGuid c g = fmap Maybe.listToMaybe $ Sql.query c (into @Sql.Query
    "select key, createdAt, deletedAt, guid, updatedAt, userAgent, userGithubId \
    \from session \
    \where deletedAt is null \
    \and guid = ? \
    \limit 1") [g]

selectByGithubId :: Sql.Connection -> Int -> IO [Model.Model Session]
selectByGithubId c i = Sql.query c (into @Sql.Query
    "select key, createdAt, deletedAt, guid, updatedAt, userAgent, userGithubId \
    \from session \
    \where userGithubId = ?") [i]

delete :: Sql.Connection -> Key.Key -> IO ()
delete connection key = do
    now <- Time.getCurrentTime
    Sql.execute
        connection
        (into @Sql.Query "update session set deletedAt = ? where key = ?")
        (now, key)
