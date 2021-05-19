module Monadoc.Type.Context where

import qualified Data.Pool as Pool
import qualified Data.Time as Time
import qualified Database.SQLite.Simple as Sql
import qualified Monadoc.Type.Config as Config
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Client.TLS as Tls

data Context = Context
    { config :: Config.Config
    , manager :: Client.Manager
    , pool :: Pool.Pool Sql.Connection
    }

fromConfig :: Config.Config -> IO Context
fromConfig c = do
    m <- Tls.newTlsManager
    p <- Pool.createPool
        (Sql.open $ Config.database c)
        Sql.close
        stripeCount
        idleTime
        resourceCount
    pure Context { config = c, manager = m, pool = p }

stripeCount :: Int
stripeCount = 1

idleTime :: Time.NominalDiffTime
idleTime = 60

resourceCount :: Int
resourceCount = 16
