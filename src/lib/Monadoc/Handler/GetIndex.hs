module Monadoc.Handler.GetIndex where

import Monadoc.Prelude

import qualified Monadoc.Handler.Common as Common
import qualified Monadoc.Model.User as User
import qualified Monadoc.Server.Response as Response
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Handler as Handler
import qualified Monadoc.Type.Route as Route
import qualified Monadoc.Type.Version as Version
import qualified Monadoc.Utility.Xml as Xml
import qualified Network.HTTP.Types as Http
import qualified Paths_monadoc as This
import qualified Text.XML as Xml

handler :: Handler.Handler
handler context request = do
    let
        config = Context.config context
        baseUrl = Config.baseUrl config
        clientId = Config.clientId config
    maybeUser <- Common.getUser context request
    pure . Response.xml Http.ok200 [] $ Xml.Document
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
                , Xml.node "user" [] [Xml.content $ maybe "" User.githubLogin maybeUser]
                , Xml.node "version" [] [Xml.content . into @String $ into @Version.Version This.version]
                ]
            , Xml.node "page" []
                [ Xml.node "index" [] []
                ]
            ])
        []
