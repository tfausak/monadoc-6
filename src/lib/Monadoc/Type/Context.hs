module Monadoc.Type.Context where

import Monadoc.Prelude

import qualified Data.Pool as Pool
import qualified Data.Time as Time
import qualified Monadoc.Vendor.Sql as Sql
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Vendor.Client as Client

data Context = Context
    { config :: Config.Config
    , manager :: Client.Manager
    , pool :: Pool.Pool Sql.Connection
    }

fromConfig :: Config.Config -> IO Context
fromConfig c = do
    m <- Client.newTlsManager
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

withConnection :: Context -> (Sql.Connection -> IO a) -> IO a
withConnection = Pool.withResource . pool
