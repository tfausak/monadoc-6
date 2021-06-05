module Monadoc.Class.ToXml where

import Monadoc.Prelude

import qualified Text.XML as Xml

class ToXml a where
    toXml :: a -> Xml.Node

instance ToXml String where
    toXml = Xml.NodeContent . into @Text

instance ToXml a => ToXml (Maybe a) where
    toXml = maybe (Xml.NodeContent $ into @Text "") toXml

instance ToXml Int where
    toXml = toXml . show
