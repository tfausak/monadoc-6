{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Type.ComponentTag where

import Monadoc.Prelude

import qualified Monadoc.Vendor.Sql as Sql
import qualified Monadoc.Class.ToXml as ToXml

data ComponentTag
    = Library
    | ForeignLibrary
    | Executable
    | TestSuite
    | Benchmark
    deriving (Eq, Ord, Show)

instance TryFrom String ComponentTag where
    tryFrom = maybeTryFrom $ \ string -> case string of
        "bench" -> Just Benchmark
        "exe" -> Just Executable
        "flib" -> Just ForeignLibrary
        "lib" -> Just Library
        "test" -> Just TestSuite
        _ -> Nothing

instance From ComponentTag String where
    from componentTag = case componentTag of
        Benchmark -> "bench"
        Executable -> "exe"
        ForeignLibrary -> "flib"
        Library -> "lib"
        TestSuite -> "test"

instance Sql.FromField ComponentTag where
    fromField = Sql.defaultFromField @String Proxy

instance Sql.ToField ComponentTag where
    toField = Sql.toField . into @String

instance ToXml.ToXml ComponentTag where
    toXml = ToXml.toXml . into @String
