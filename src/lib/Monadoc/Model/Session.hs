module Monadoc.Model.Session where

import qualified Data.Maybe as Maybe
import qualified Data.Time as Time
import qualified Database.SQLite.Simple as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import qualified Monadoc.Type.Guid as Guid
import qualified Monadoc.Utility.Convert as Convert

data Session = Session
    { createdAt :: Time.UTCTime
    , deletedAt :: Maybe Time.UTCTime
    , guid :: Guid.Guid
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

instance Sql.ToRow Session where
    toRow session =
        [ Sql.toField $ createdAt session
        , Sql.toField $ deletedAt session
        , Sql.toField $ guid session
        , Sql.toField $ userAgent session
        , Sql.toField $ userGithubId session
        ]

insert :: Sql.Connection -> Session -> IO ()
insert c = Sql.execute c $ Convert.stringToQuery
    "insert into session \
    \(createdAt, deletedAt, guid, userAgent, userGithubId) \
    \values (?, ?, ?, ?, ?)"

selectByGuid :: Sql.Connection -> Guid.Guid -> IO (Maybe Session)
selectByGuid c g = fmap Maybe.listToMaybe $ Sql.query c (Convert.stringToQuery
    "select createdAt, deletedAt, guid, userAgent, userGithubId \
    \from session \
    \where deletedAt is null \
    \and guid = ? \
    \limit 1") [g]
