{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Type.ComponentId where

import qualified Monadoc.Type.ComponentName as ComponentName
import qualified Monadoc.Type.ComponentTag as ComponentTag
import qualified Monadoc.Utility.Either as Either
import qualified Witch

data ComponentId = ComponentId
    { tag :: ComponentTag.ComponentTag
    , name :: Maybe ComponentName.ComponentName
    } deriving (Eq, Show)

instance Witch.TryFrom String ComponentId where
    tryFrom = Witch.maybeTryFrom $ \ string -> do
        let (before, after) = break (== ':') string
        t <- Either.toMaybe $ Witch.tryFrom @String before
        case after of
            "" -> pure ComponentId { tag = t, name = Nothing }
            ':' : rest -> do
                n <- Either.toMaybe $ Witch.tryFrom @String rest
                pure ComponentId { tag = t, name = Just n }
            _ -> Nothing

instance Witch.From ComponentId String where
    from componentId = Witch.into @String (tag componentId) <> case name componentId of
        Nothing -> ""
        Just n -> ':' : Witch.into @String n
