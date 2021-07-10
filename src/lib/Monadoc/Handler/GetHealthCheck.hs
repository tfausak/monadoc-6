{-# LANGUAGE TypeApplications #-}

module Monadoc.Handler.GetHealthCheck where

import Monadoc.Prelude

import qualified Monadoc.Server.Response as Response
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Handler as Handler
import qualified Monadoc.Vendor.Sql as Sql
import qualified Network.HTTP.Types as Http

handler :: Handler.Handler
handler context _ = do
    rows <- Context.withConnection context $ \ connection ->
        Sql.query_ connection "select 1"
    guard $ rows == [Sql.Only @Int 1]
    pure $ Response.status Http.ok200 []
