module Monadoc.Model.User where

import qualified Data.Time as Time
import qualified Database.SQLite.Simple as Sql
import qualified Database.SQLite.Simple.ToField as Sql

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
