module Monadoc.Model.User where

import qualified Data.Maybe as Maybe
import qualified Data.Time as Time
import qualified Database.SQLite.Simple as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import qualified Monadoc.Utility.Convert as Convert

data User = User
    { createdAt :: Time.UTCTime
    , deletedAt :: Maybe Time.UTCTime
    , githubId :: Int
    , githubLogin :: String
    , githubToken :: String
    , updatedAt :: Time.UTCTime
    } deriving (Eq, Show)

instance Sql.FromRow User where
    fromRow = User
        <$> Sql.field
        <*> Sql.field
        <*> Sql.field
        <*> Sql.field
        <*> Sql.field
        <*> Sql.field

instance Sql.ToRow User where
    toRow user =
        [ Sql.toField $ createdAt user
        , Sql.toField $ deletedAt user
        , Sql.toField $ githubId user
        , Sql.toField $ githubLogin user
        , Sql.toField $ githubToken user
        , Sql.toField $ updatedAt user
        ]

insertOrUpdate :: Sql.Connection -> User -> IO ()
insertOrUpdate c = Sql.execute c $ Convert.stringToQuery
    "insert into user \
    \(createdAt, deletedAt, githubId, githubLogin, githubToken, updatedAt) \
    \values (?, ?, ?, ?, ?, ?) \
    \on conflict (githubId) do update \
    \set deletedAt = excluded.deletedAt, \
    \githubLogin = excluded.githubLogin, \
    \githubToken = excluded.githubToken, \
    \updatedAt = excluded.updatedAt"

selectByGithubId :: Sql.Connection -> Int -> IO (Maybe User)
selectByGithubId c i = fmap Maybe.listToMaybe $ Sql.query c (Convert.stringToQuery
    "select createdAt, deletedAt, githubId, githubLogin, githubToken, updatedAt \
    \from user \
    \where deletedAt is null \
    \and githubId = ? \
    \limit 1") [i]
