module Monadoc.Class.ToXml where

import Monadoc.Prelude

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
