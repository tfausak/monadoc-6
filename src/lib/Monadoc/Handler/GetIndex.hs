module Monadoc.Handler.GetIndex where

import Monadoc.Prelude

import qualified Data.Maybe as Maybe
import qualified Data.Pool as Pool
import qualified Data.UUID as Uuid
import qualified Monadoc.Model.Session as Session
import qualified Monadoc.Model.User as User
import qualified Monadoc.Server.Response as Response
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Handler as Handler
import qualified Monadoc.Type.Route as Route
import qualified Monadoc.Utility.Convert as Convert
import qualified Monadoc.Utility.Xml as Xml
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Paths_monadoc as Package
import qualified Text.XML as Xml
import qualified Web.Cookie as Cookie

getUser :: Context.Context -> Wai.Request -> IO (Maybe User.User)
getUser context request = do
    let
        cookies = Cookie.parseCookies <<< Maybe.fromMaybe mempty <<< lookup Http.hCookie $ Wai.requestHeaders request
    case lookup (into @ByteString "guid") cookies of
        Just byteString -> case fmap (from @Uuid.UUID) $ Uuid.fromASCIIBytes byteString of
            Just guid -> Pool.withResource (Context.pool context) $ \ connection -> do
                    maybeSession <- Session.selectByGuid connection guid
                    case maybeSession of
                        Just session -> User.selectByGithubId connection $ Session.userGithubId session
                        Nothing -> pure Nothing
            _ -> pure Nothing
        _ -> pure Nothing

handler :: Handler.Handler
handler context request = do
    let
        config = Context.config context
        baseUrl = Config.baseUrl config
        clientId = Config.clientId config
    maybeUser <- getUser context request
    pure <<< Response.xml Http.ok200 [] $ Xml.Document
        (Xml.Prologue
            [Xml.MiscInstruction $ Xml.Instruction
                (into @Text "xml-stylesheet")
                (into @Text $ "type=\"text/xsl\" charset=\"UTF-8\" href=\"" <> Xml.escape (baseUrl <> Route.toString Route.Template) <> "\"")]
            Nothing
            [])
        (Xml.element "monadoc" []
            [ Xml.node "config" []
                [ Xml.node "baseUrl" [] [Xml.content baseUrl]
                , Xml.node "clientId" [] [Xml.content clientId]
                , Xml.node "version" [] [Xml.content $ Convert.versionToString Package.version]
                , Xml.node "user" [] [Xml.content $ maybe "" User.githubLogin maybeUser]
                ]
            ])
        []
