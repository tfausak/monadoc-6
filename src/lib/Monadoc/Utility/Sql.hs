module Monadoc.Utility.Sql where

import Monadoc.Prelude

import qualified Control.Monad.Catch as Exception
import qualified Control.Retry as Retry
import qualified Data.Proxy as Proxy
import qualified Data.Typeable as Typeable
import qualified Database.SQLite.Simple as Sql
import qualified Database.SQLite.Simple.FromField as Sql
import qualified Monadoc.Utility.Log as Log

defaultFromField
    :: ( Sql.FromField s
        , Show s
        , TryFrom s t
        , Typeable.Typeable s
        , Typeable.Typeable t
        )
    => proxy s
    -> Sql.FieldParser t
defaultFromField p f = do
    s <- Sql.fromField f
    case tryFrom $ Proxy.asProxyTypeOf s p of
        Left e -> Sql.returnError Sql.ConversionFailed f $ show e
        Right t -> pure t

execute
    :: Sql.ToRow i
    => Sql.Connection
    -> String
    -> i
    -> IO ()
execute connection sql = void . query @_ @[Sql.SQLData] connection sql

execute_ :: Sql.Connection -> String -> IO ()
execute_ connection sql = execute connection sql ()

query
    :: (Sql.ToRow i, Sql.FromRow o)
    => Sql.Connection
    -> String
    -> i
    -> IO [o]
query connection sql input = Retry.recovering
    Retry.retryPolicyDefault
    [always . Exception.Handler $ pure . (== Sql.ErrorBusy) . Sql.sqlError]
    (\ retryStatus -> do
        Log.info $ "[sql] " <> sql
        when (Retry.rsIterNumber retryStatus > 0) . Log.info $ "[retry] " <> show retryStatus
        Sql.query connection (Sql.Query $ into @Text sql) input)

query_ :: Sql.FromRow o => Sql.Connection -> String -> IO [o]
query_ connection sql = query connection sql ()
