{-# LANGUAGE TypeApplications #-}

module Monadoc.Utility.Aeson where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Text as Text
import qualified Witch

required :: Aeson.FromJSON a => Aeson.Object -> String -> Aeson.Parser a
required o k = o Aeson..: Witch.into @Text.Text k
