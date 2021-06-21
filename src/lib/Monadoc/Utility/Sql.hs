module Monadoc.Utility.Sql where

import Monadoc.Prelude

import qualified Control.Monad.Catch as Exception
import qualified Control.Retry as Retry
import qualified Data.Proxy as Proxy
import qualified Data.Typeable as Typeable
import qualified Database.SQLite.Simple as Sql
import qualified Database.SQLite.Simple.FromField as Sql
import qualified GHC.Clock as Clock
import qualified Monadoc.Type.RequestId as RequestId
import qualified Monadoc.Utility.Log as Log
import qualified Text.Printf as Printf

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
query connection sql input = do
    requestId <- RequestId.random
    before <- Clock.getMonotonicTime
    result <- Retry.recovering
        Retry.retryPolicyDefault
        [always . Exception.Handler $ pure . (== Sql.ErrorBusy) . Sql.sqlError]
        $ \ retryStatus -> do
            let number = Retry.rsIterNumber retryStatus
            when (number > 0) . Log.info $ Printf.printf "[sql/%04x] [retry/%d] %s"
                (into @Word16 requestId)
                number
                (show retryStatus)
            Sql.query connection (Sql.Query $ into @Text sql) input
    after <- Clock.getMonotonicTime
    Log.info $ Printf.printf "[sql/%04x] %s -- %.3f"
        (into @Word16 requestId)
        sql
        (after - before)
    pure result

query_ :: Sql.FromRow o => Sql.Connection -> String -> IO [o]
query_ connection sql = query connection sql ()
