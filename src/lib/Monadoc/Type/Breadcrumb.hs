module Monadoc.Type.Breadcrumb where

import Monadoc.Prelude

import qualified Monadoc.Class.ToXml as ToXml
import qualified Monadoc.Type.Route as Route
import qualified Monadoc.Utility.Xml as Xml

data Breadcrumb = Breadcrumb
    { name :: String
    , route :: Maybe Route.Route
    } deriving (Eq, Show)

instance ToXml.ToXml Breadcrumb where
    toXml breadcrumb = Xml.node "breadcrumb" []
        [ Xml.node "name" [] [ToXml.toXml $ name breadcrumb]
        , Xml.node "route" [] [ToXml.toXml $ route breadcrumb]
        ]
