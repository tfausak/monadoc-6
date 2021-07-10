module Monadoc.Handler.GetAccount where

import qualified Control.Monad.Catch as Exception
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Ord as Ord
import qualified Monadoc.Class.ToXml as ToXml
import qualified Monadoc.Exception.Forbidden as Forbidden
import qualified Monadoc.Handler.Common as Common
import qualified Monadoc.Model.Session as Session
import qualified Monadoc.Model.User as User
import qualified Monadoc.Type.Breadcrumb as Breadcrumb
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Handler as Handler
import qualified Monadoc.Type.Meta as Meta
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Type.Root as Root
import qualified Monadoc.Type.Route as Route
import qualified Monadoc.Utility.Xml as Xml

handler :: Handler.Handler
handler context request = do
    maybeUser <- Common.getUser context request
    user <- maybe (Exception.throwM Forbidden.new) pure maybeUser
    sessions <- Context.withConnection context $ \ connection ->
        Session.selectByGithubId connection . User.githubId $ Model.value user
    let
        route = Route.Account
        breadcrumbs =
            [ Breadcrumb.Breadcrumb
                { Breadcrumb.name = "Home"
                , Breadcrumb.route = Just Route.Index
                }
            , Breadcrumb.Breadcrumb
                { Breadcrumb.name = "Account"
                , Breadcrumb.route = Nothing
                }
            ]
        page = Xml.node "page" []
            [ Xml.node "account" []
                [ Xml.node "name" [] [ToXml.toXml . User.githubLogin . Model.value $ user]
                , Xml.node "sessions" []
                . fmap (\ x -> Xml.node "session" []
                    [ Xml.node "createdAt" [] [ToXml.toXml $ Session.createdAt x]
                    , Xml.node "guid" [] [ToXml.toXml $ Session.guid x]
                    , Xml.node "userAgent" [] [ToXml.toXml $ Session.userAgent x]
                    ])
                . List.sortOn (Ord.Down . Session.createdAt)
                . filter (Maybe.isNothing . Session.deletedAt)
                . fmap Model.value
                $ sessions
                ]
            ]
    pure $ Common.makeResponse Root.Root
        { Root.meta = (Meta.fromContext context route)
            { Meta.breadcrumbs = breadcrumbs
            , Meta.title = "Monadoc - Account"
            , Meta.user = fmap (User.githubLogin . Model.value) maybeUser
            }
        , Root.page = page
        }
