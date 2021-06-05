module Monadoc.Handler.GetPackage where

import Monadoc.Prelude

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Ord as Ord
import qualified Data.Pool as Pool
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

handler :: PackageName.PackageName -> Handler.Handler
handler packageName context request = do
    let
        config = Context.config context
        baseUrl = Config.baseUrl config
        clientId = Config.clientId config
    maybeUser <- Common.getUser context request
    packages <- Pool.withResource (Context.pool context) $ \ connection ->
        Package.selectByName connection packageName
    case packages of
        [] -> pure $ Response.status Http.notFound404 []
        package : _ -> pure . Response.xml Http.ok200 [] $ Xml.Document
            (Xml.Prologue
                [Xml.MiscInstruction $ Xml.Instruction
                    (into @Text "xml-stylesheet")
                    (into @Text $ "type=\"text/xsl\" charset=\"UTF-8\" href=\"" <> Xml.escape (baseUrl <> Route.toString Route.Template) <> "\"")]
                Nothing
                [])
            (Xml.element "monadoc" []
                [ Xml.node "config" []
                    [ Xml.node "baseUrl" [] [ToXml.toXml baseUrl]
                    , Xml.node "clientId" [] [ToXml.toXml clientId]
                    , Xml.node "user" [] [ToXml.toXml $ fmap User.githubLogin maybeUser]
                    , Xml.node "version" [] [ToXml.toXml $ into @Version.Version This.version]
                    ]
                , Xml.node "page" []
                    [ Xml.node "package" []
                        [ Xml.node "name" [] [ToXml.toXml $ Package.name package]
                        , Xml.node "versions" []
                        . fmap (\ (ver, pkgs) -> Xml.node "version" []
                            [ Xml.node "number" [] [ToXml.toXml ver]
                            , Xml.node "revisions" []
                            . fmap (\ pkg -> Xml.node "revision" [] [ToXml.toXml $ Package.revision pkg])
                            $ List.sortOn (Ord.Down . Package.revision) pkgs
                            ])
                        . Map.toDescList
                        $ groupBy Package.version packages
                        ]
                    ]
                ])
            []

groupBy :: (Foldable t, Ord k) => (v -> k) -> t v -> Map k [v]
groupBy f = foldr (\ v -> Map.alter (Just . maybe [v] (v :)) $ f v) Map.empty
