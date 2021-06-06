module Monadoc.Class.ToXml where

import Monadoc.Prelude

import qualified Data.Time as Time
import qualified Text.XML as Xml

class ToXml a where
    toXml :: a -> Xml.Node

instance ToXml String where
    toXml = toXml . into @Text

instance ToXml a => ToXml (Maybe a) where
    toXml = maybe (toXml $ into @Text "") toXml

instance ToXml Int where
    toXml = toXml . show

instance ToXml Word where
    toXml = toXml . show

instance ToXml Text where
    toXml = Xml.NodeContent

instance ToXml Time.UTCTime where
    toXml = toXml . Time.formatTime Time.defaultTimeLocale "%Y-%m-%dT%H:%M:%S%3QZ"
