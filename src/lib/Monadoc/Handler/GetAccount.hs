module Monadoc.Handler.GetAccount where

import Monadoc.Prelude

import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Ord as Ord
import qualified Data.Time as Time
import qualified Monadoc.Class.ToXml as ToXml
import qualified Monadoc.Exception.Forbidden as Forbidden
import qualified Monadoc.Handler.Common as Common
import qualified Monadoc.Model.Session as Session
import qualified Monadoc.Model.User as User
import qualified Monadoc.Type.Breadcrumb as Breadcrumb
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Guid as Guid
import qualified Monadoc.Type.Handler as Handler
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Type.Route as Route
import qualified Monadoc.Utility.Xml as Xml

handler :: Handler.Handler
handler context request = do
    let route = Route.Account
    maybeUser <- Common.getUser context request
    user <- case maybeUser of
        Nothing -> throwM Forbidden.new
        Just user -> pure user
    sessions <- Context.withConnection context $ \ connection ->
        Session.selectByGithubId connection . User.githubId $ Model.value user
    pure $ Common.makeResponse Common.Monadoc
        { Common.monadoc_config = (Common.config_fromContext context route)
            { Common.config_breadcrumbs =
                [ Breadcrumb.Breadcrumb
                    { Breadcrumb.name = "Home"
                    , Breadcrumb.route = Just Route.Index
                    }
                , Breadcrumb.Breadcrumb
                    { Breadcrumb.name = "Account"
                    , Breadcrumb.route = Nothing
                    }
                ]
            , Common.config_user = fmap (User.githubLogin . Model.value) maybeUser
            }
        , Common.monadoc_page = Account
            { account_name = User.githubLogin $ Model.value user
            , account_sessions = fmap (\ session -> Session
                { session_createdAt = Session.createdAt session
                , session_guid = Session.guid session
                , session_userAgent = Session.userAgent session
                })
                . List.sortOn (Ord.Down . Session.createdAt)
                . filter (Maybe.isNothing . Session.deletedAt)
                $ fmap Model.value sessions
            }
        }

data Account = Account
    { account_name :: String
    , account_sessions :: [Session]
    } deriving (Eq, Show)

instance ToXml.ToXml Account where
    toXml account = Xml.node "account" []
        [ Xml.node "name" [] [ToXml.toXml $ account_name account]
        , Xml.node "sessions" [] . fmap ToXml.toXml $ account_sessions account
        ]

data Session = Session
    { session_createdAt :: Time.UTCTime
    , session_guid :: Guid.Guid
    , session_userAgent :: String
    } deriving (Eq, Show)

instance ToXml.ToXml Session where
    toXml session = Xml.node "session" []
        [ Xml.node "createdAt" [] [ToXml.toXml $ session_createdAt session]
        , Xml.node "guid" [] [ToXml.toXml $ session_guid session]
        , Xml.node "userAgent" [] [ToXml.toXml $ session_userAgent session]
        ]
