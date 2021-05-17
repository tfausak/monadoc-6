module Monadoc.Type.Context where

import qualified Data.Pool as Pool
import qualified Data.Time as Time
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
        stripeCount
        idleTime
        resourceCount
    pure Context { config = c, pool = p }

stripeCount :: Int
stripeCount = 1

idleTime :: Time.NominalDiffTime
idleTime = 60

resourceCount :: Int
resourceCount = 16
