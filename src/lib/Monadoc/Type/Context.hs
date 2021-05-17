module Monadoc.Type.Context where

import qualified Data.Pool as Pool
import qualified Database.SQLite.Simple as Sql
import qualified Monadoc.Type.Config as Config

data Context = Context
    { config :: Config.Config
    , pool :: Pool.Pool Sql.Connection
    } deriving Show

fromConfig :: Config.Config -> IO Context
fromConfig c = do
    p <- Pool.createPool
        (Sql.open $ Config.database c)
        Sql.close
        1 -- stripe count
        60 -- seconds
        16 -- resource count
    pure Context { config = c, pool = p }
