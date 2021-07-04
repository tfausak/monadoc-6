module Monadoc.Job.UpsertHackageIndex where

import Monadoc.Prelude

import qualified Monadoc.Job.InsertHackageIndex as InsertHackageIndex
import qualified Monadoc.Job.UpdateHackageIndex as UpdateHackageIndex
import qualified Monadoc.Model.HackageIndex as HackageIndex
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Utility.Log as Log

run :: Context.Context -> IO HackageIndex.HackageIndex
run context = do
    Log.info "[worker] refreshing hackage index"
    maybeHackageIndex <- Context.withConnection context HackageIndex.select
    case maybeHackageIndex of
        Nothing -> InsertHackageIndex.run context
        Just hackageIndex -> UpdateHackageIndex.run context hackageIndex
