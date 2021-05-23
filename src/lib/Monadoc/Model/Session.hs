module Monadoc.Model.Session where

import qualified Data.Time as Time
import qualified Database.SQLite.Simple as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import qualified Monadoc.Type.Guid as Guid

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
