module Monadoc.Type.Routes where

import Monadoc.Prelude

import qualified Monadoc.Class.ToXml as ToXml
import qualified Monadoc.Type.Route as Route
import qualified Monadoc.Utility.Xml as Xml

data Routes = Routes
    { account :: Route.Route
    , bootstrap :: Route.Route
    , callback :: Route.Route
    , favicon :: Route.Route
    , logOut :: Route.Route
    , revoke :: Route.Route
    , search :: Route.Route
    , self :: Route.Route
    } deriving (Eq, Show)

instance ToXml.ToXml Routes where
    toXml routes = Xml.node "routes" []
        [ Xml.node "account" [] [ToXml.toXml $ account routes]
        , Xml.node "bootstrap" [] [ToXml.toXml $ bootstrap routes]
        , Xml.node "callback" [] [ToXml.toXml $ callback routes]
        , Xml.node "favicon" [] [ToXml.toXml $ favicon routes]
        , Xml.node "logOut" [] [ToXml.toXml $ logOut routes]
        , Xml.node "revoke" [] [ToXml.toXml $ revoke routes]
        , Xml.node "search" [] [ToXml.toXml $ search routes]
        , Xml.node "self" [] [ToXml.toXml $ self routes]
        ]

fromRoute :: Route.Route -> Routes
fromRoute self = Routes
    { account = Route.Account
    , bootstrap = Route.Bootstrap
    , callback = Route.Callback
    , favicon = Route.Favicon
    , logOut = Route.LogOut
    , revoke = Route.Revoke
    , search = Route.Search Nothing
    , self
    }
