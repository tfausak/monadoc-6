module Monadoc.Type.Context where

import Monadoc.Prelude

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
fromConfig config = do
    manager <- Tls.newTlsManager
    pool <- Pool.createPool
        (Sql.open <| Config.database config)
        Sql.close
        stripeCount
        idleTime
        resourceCount
    pure Context { config, manager, pool }

stripeCount :: Int
stripeCount = 1

idleTime :: Time.NominalDiffTime
idleTime = 60

resourceCount :: Int
resourceCount = 16
