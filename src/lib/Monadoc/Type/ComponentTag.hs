{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Type.ComponentTag where

import Monadoc.Prelude

import qualified Data.Proxy as Proxy
import qualified Monadoc.Class.ToXml as ToXml
import qualified Monadoc.Vendor.Sql as Sql
import qualified Witch

data ComponentTag
    = Library
    | ForeignLibrary
    | Executable
    | TestSuite
    | Benchmark
    deriving (Eq, Ord, Show)

instance Witch.TryFrom String ComponentTag where
    tryFrom = Witch.maybeTryFrom $ \ string -> case string of
        "bench" -> Just Benchmark
        "exe" -> Just Executable
        "flib" -> Just ForeignLibrary
        "lib" -> Just Library
        "test" -> Just TestSuite
        _ -> Nothing

instance Witch.From ComponentTag String where
    from componentTag = case componentTag of
        Benchmark -> "bench"
        Executable -> "exe"
        ForeignLibrary -> "flib"
        Library -> "lib"
        TestSuite -> "test"

instance Sql.FromField ComponentTag where
    fromField = Sql.defaultFromField @String Proxy.Proxy

instance Sql.ToField ComponentTag where
    toField = Sql.toField . Witch.into @String

instance ToXml.ToXml ComponentTag where
    toXml = ToXml.toXml . Witch.into @String
