{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Monadoc.Prelude.Orphanage where

import qualified Data.Text
import qualified Distribution.Utils.ShortText
import qualified Prelude
import qualified Witch

instance Witch.From Prelude.String Distribution.Utils.ShortText.ShortText where
    from = Distribution.Utils.ShortText.toShortText

instance Witch.From Distribution.Utils.ShortText.ShortText Prelude.String where
    from = Distribution.Utils.ShortText.fromShortText

instance Witch.From Data.Text.Text Distribution.Utils.ShortText.ShortText where
    from = Witch.via @Prelude.String

instance Witch.From Distribution.Utils.ShortText.ShortText Data.Text.Text where
    from = Witch.via @Prelude.String
