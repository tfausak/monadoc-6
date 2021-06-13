module Monadoc.Type.ComponentId where

import Monadoc.Prelude

import qualified Monadoc.Type.ComponentName as ComponentName
import qualified Monadoc.Type.ComponentTag as ComponentTag

data ComponentId = ComponentId
    { tag :: ComponentTag.ComponentTag
    , name :: Maybe ComponentName.ComponentName
    } deriving (Eq, Show)

instance TryFrom String ComponentId where
    tryFrom = maybeTryFrom $ \ string -> do
        let (before, after) = break (== ':') string
        tag <- hush $ tryFrom @String before
        case after of
            "" -> pure ComponentId { tag, name = Nothing }
            ':' : rest -> do
                name <- hush $ tryFrom @String rest
                pure ComponentId { tag, name = Just name }
            _ -> Nothing

instance From ComponentId String where
    from componentId = into @String (tag componentId) <> case name componentId of
        Nothing -> ""
        Just n -> ':' : into @String n
