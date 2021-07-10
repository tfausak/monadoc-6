{-# LANGUAGE TypeApplications #-}

module Monadoc.Handler.Common where

import Monadoc.Prelude

import qualified Data.ByteString as ByteString
import qualified Data.CaseInsensitive as CI
import qualified Data.UUID as Uuid
import qualified Monadoc.Class.ToXml as ToXml
import qualified Monadoc.Model.Session as Session
import qualified Monadoc.Model.User as User
import qualified Monadoc.Server.Response as Response
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Guid as Guid
import qualified Monadoc.Type.Meta as Meta
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Type.Root as Root
import qualified Monadoc.Type.Route as Route
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Web.Cookie as Cookie

getUser :: Context.Context -> Wai.Request -> IO (Maybe (Model.Model User.User))
getUser context request = do
    maybeSession <- getSession context request
    case maybeSession of
        Nothing -> pure Nothing
        Just session -> getUserWith context session

getUserWith :: Context.Context -> Model.Model Session.Session -> IO (Maybe (Model.Model User.User))
getUserWith context session =
    Context.withConnection context $ \ connection ->
        User.selectByGithubId connection . Session.userGithubId $ Model.value session

getSession :: Context.Context -> Wai.Request -> IO (Maybe (Model.Model Session.Session))
getSession context request =
    case getSessionGuid request of
        Nothing -> pure Nothing
        Just guid -> getSessionWith context guid

getSessionGuid :: Wai.Request -> Maybe Guid.Guid
getSessionGuid request = do
    cookies <- lookup Http.hCookie $ Wai.requestHeaders request
    byteString <- lookup (into @ByteString.ByteString "guid") $ Cookie.parseCookies cookies
    fmap (into @Guid.Guid) $ Uuid.fromASCIIBytes byteString

getSessionWith :: Context.Context -> Guid.Guid -> IO (Maybe (Model.Model Session.Session))
getSessionWith context guid =
    Context.withConnection context $ \ connection ->
        Session.selectByGuid connection guid

makeResponse :: ToXml.ToXml page => Root.Root page -> Wai.Response
makeResponse monadoc =
    let
        baseUrl = Meta.baseUrl $ Root.meta monadoc
        status = Http.ok200
        hLink = CI.mk $ into @ByteString.ByteString "Link"
        link = into @ByteString.ByteString $ "<" <> baseUrl <> Route.toString Route.Bootstrap <> ">; rel=preload; as=style"
        headers = [(hLink, link)]
    in Response.xml status headers $ Root.toDocument monadoc
