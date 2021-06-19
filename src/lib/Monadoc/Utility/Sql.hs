module Monadoc.Utility.Sql where

import Monadoc.Prelude

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
execute connection sql input = do
    Log.info $ "[sql] " <> sql
    Sql.execute connection (Sql.Query $ into @Text sql) input

execute_ :: Sql.Connection -> String -> IO ()
execute_ connection sql = execute connection sql ()

query
    :: (Sql.ToRow i, Sql.FromRow o)
    => Sql.Connection
    -> String
    -> i
    -> IO [o]
query connection sql input = do
    Log.info $ "[sql] " <> sql
    Sql.query connection (Sql.Query $ into @Text sql) input

query_ :: Sql.FromRow o => Sql.Connection -> String -> IO [o]
query_ connection sql = query connection sql ()
