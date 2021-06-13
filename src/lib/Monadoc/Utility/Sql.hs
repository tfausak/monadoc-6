module Monadoc.Utility.Sql where

import Monadoc.Prelude

import qualified Data.Proxy as Proxy
import qualified Data.Typeable as Typeable
import qualified Database.SQLite.Simple.FromField as Sql

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
