module Monadoc.Handler.GetVersion where

import Monadoc.Prelude

import qualified Data.CaseInsensitive as CI
import qualified Data.Pool as Pool
import qualified Data.Set as Set
import qualified Monadoc.Class.ToXml as ToXml
import qualified Monadoc.Handler.Common as Common
import qualified Monadoc.Model.Package as Package
import qualified Monadoc.Model.User as User
import qualified Monadoc.Server.Response as Response
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Handler as Handler
import qualified Monadoc.Type.PackageName as PackageName
import qualified Monadoc.Type.Route as Route
import qualified Monadoc.Type.Version as Version
import qualified Monadoc.Utility.Xml as Xml
import qualified Network.HTTP.Types as Http
import qualified Paths_monadoc as This
import qualified Text.XML as Xml

handler :: PackageName.PackageName -> Version.Version -> Handler.Handler
handler packageName version context request = do
    let
        config = Context.config context
        baseUrl = Config.baseUrl config
        clientId = Config.clientId config
    maybeUser <- Common.getUser context request
    packages <- Pool.withResource (Context.pool context) $ \ connection ->
        Package.selectByNameAndVersion connection packageName version
    case packages of
        [] -> pure $ Response.status Http.notFound404 []
        _ : _ -> pure . Response.xml Http.ok200
            [(CI.mk $ into @ByteString "Link", into @ByteString $ "<" <> baseUrl <> Route.toString Route.Bootstrap <> ">; rel=preload; as=style")]
            $ Xml.Document
            (Xml.Prologue
                [Xml.MiscInstruction $ Xml.Instruction
                    (into @Text "xml-stylesheet")
                    (into @Text $ "type=\"text/xsl\" charset=\"UTF-8\" href=\"" <> Xml.escape (baseUrl <> Route.toString Route.Template) <> "\"")]
                Nothing
                [])
            (Xml.element "monadoc" []
                [ Xml.node "config" []
                    [ Xml.node "baseUrl" [] [ToXml.toXml baseUrl]
                    , Xml.node "breadcrumbs" []
                        [ Xml.node "breadcrumb" [] [Xml.node "name" [] [ToXml.toXml "Home"], Xml.node "link" [] [ToXml.toXml $ baseUrl <> Route.toString Route.Index]]
                        , Xml.node "breadcrumb" [] [Xml.node "name" [] [ToXml.toXml packageName], Xml.node "link" [] [ToXml.toXml $ baseUrl <> Route.toString (Route.Package packageName)]]
                        , Xml.node "breadcrumb" [] [Xml.node "name" [] [ToXml.toXml version]]
                        ]
                    , Xml.node "clientId" [] [ToXml.toXml clientId]
                    , Xml.node "user" [] [ToXml.toXml $ fmap User.githubLogin maybeUser]
                    , Xml.node "version" [] [ToXml.toXml $ into @Version.Version This.version]
                    ]
                , Xml.node "page" []
                    [ Xml.node "version" []
                        [ Xml.node "name" [] [ToXml.toXml packageName]
                        , Xml.node "version" [] [ToXml.toXml version]
                        , Xml.node "revisions" []
                        . fmap (\ rev -> Xml.node "revision" [] [ToXml.toXml rev])
                        . Set.toDescList
                        . Set.fromList
                        $ fmap Package.revision packages
                        ]
                    ]
                ])
            []
