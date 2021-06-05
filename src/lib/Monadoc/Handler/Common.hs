module Monadoc.Handler.Common where

import Monadoc.Prelude

import qualified Data.Maybe as Maybe
import qualified Data.Pool as Pool
import qualified Data.UUID as Uuid
import qualified Monadoc.Model.Session as Session
import qualified Monadoc.Model.User as User
import qualified Monadoc.Type.Context as Context
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Web.Cookie as Cookie

getUser :: Context.Context -> Wai.Request -> IO (Maybe User.User)
getUser context request = do
    let
        cookies = Cookie.parseCookies . Maybe.fromMaybe mempty . lookup Http.hCookie $ Wai.requestHeaders request
    case lookup (into @ByteString "guid") cookies of
        Just byteString -> case fmap (from @Uuid.UUID) $ Uuid.fromASCIIBytes byteString of
            Just guid -> Pool.withResource (Context.pool context) $ \ connection -> do
                    maybeSession <- Session.selectByGuid connection guid
                    case maybeSession of
                        Just session -> User.selectByGithubId connection $ Session.userGithubId session
                        Nothing -> pure Nothing
            _ -> pure Nothing
        _ -> pure Nothing
