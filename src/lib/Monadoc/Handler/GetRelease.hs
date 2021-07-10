module Monadoc.Handler.GetRelease where

import Monadoc.Prelude

import qualified Monadoc.Class.ToXml as ToXml
import qualified Monadoc.Exception.Found as Found
import qualified Monadoc.Exception.NotFound as NotFound
import qualified Monadoc.Handler.Common as Common
import qualified Monadoc.Model.Package as Package
import qualified Monadoc.Model.User as User
import qualified Monadoc.Type.Breadcrumb as Breadcrumb
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Handler as Handler
import qualified Monadoc.Type.Meta as Meta
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Type.PackageName as PackageName
import qualified Monadoc.Type.Release as Release
import qualified Monadoc.Type.Root as Root
import qualified Monadoc.Type.Route as Route
import qualified Monadoc.Utility.Xml as Xml

import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Ord as Ord
import qualified Documentation.Haddock.Parser as Haddock
import qualified Documentation.Haddock.Types as Haddock
import qualified Monadoc.Model.Component as Component
import qualified Monadoc.Model.Distribution as Distribution
import qualified Monadoc.Model.File as File
import qualified Monadoc.Model.LatestVersion as LatestVersion
import qualified Monadoc.Model.PreferredVersions as PreferredVersions
import qualified Monadoc.Model.SourceRepository as SourceRepository
import qualified Monadoc.Type.ComponentId as ComponentId
import qualified Monadoc.Type.ComponentName as ComponentName
import qualified Monadoc.Type.ComponentTag as ComponentTag
import qualified Monadoc.Type.Revision as Revision
import qualified Monadoc.Type.Version as Version
import qualified Monadoc.Type.VersionRange as VersionRange

handler
    :: PackageName.PackageName
    -> Release.Release
    -> Handler.Handler
handler packageName release context request = do
    let version = Release.version release
    revision <- case Release.revision release of
        Just revision -> pure revision
        Nothing -> do
            maybeRevision <- Context.withConnection context $ \ connection ->
                Package.selectLatestRevision connection packageName version
            case maybeRevision of
                Nothing -> throwM NotFound.new
                Just revision -> throwM
                    . Found.new
                    . Route.toString
                    $ Route.Release packageName release { Release.revision = Just revision }

    maybeUser <- Common.getUser context request
    maybePackage <- Context.withConnection context $ \ connection ->
        Package.select connection packageName version revision
    package <- maybe (throwM NotFound.new) pure maybePackage
    packages <- Context.withConnection context $ \ connection ->
        Package.selectByName connection packageName
    let
        sortedPackages = packages
            & fmap Model.value
            & List.sortOn (\ x -> Ord.Down (Package.version x, Package.revision x))
    maybePreferredVersions <- Context.withConnection context $ \ connection ->
        PreferredVersions.selectByPackageName connection packageName
    let versionRange = maybe VersionRange.any (PreferredVersions.versionRange . Model.value) maybePreferredVersions
    components <- Context.withConnection context $ \ connection ->
        Component.selectByPackage connection $ Model.key package
    sourceRepositories <- Context.withConnection context $ \ connection ->
        SourceRepository.selectByPackage connection $ Model.key package
    maybeDistribution <- Context.withConnection context $ \ connection ->
        Distribution.selectByPackageAndVersion connection packageName version
    distribution <- maybe (throwM NotFound.new) pure maybeDistribution
    files <- Context.withConnection context $ \ connection ->
        File.selectByDistribution connection $ Model.key distribution
    maybeLatestVersion <- Context.withConnection context $ \ connection ->
        LatestVersion.selectByPackage connection packageName
    let
        route = Route.Release packageName release
        breadcrumbs =
            [ Breadcrumb.Breadcrumb
                { Breadcrumb.name = "Home"
                , Breadcrumb.route = Just Route.Index
                }
            , Breadcrumb.Breadcrumb
                { Breadcrumb.name = into @String packageName
                , Breadcrumb.route = Just $ Route.Package packageName
                }
            , Breadcrumb.Breadcrumb
                { Breadcrumb.name = into @String release
                , Breadcrumb.route = Nothing
                }
            ]
        prefix = mconcat
            [ into @String packageName
            , "-"
            , into @String version
            , "/"
            ]
        page = Xml.node "release" []
            [ Xml.node "package" []
                [ Xml.node "author" [] [ToXml.toXml . Package.author $ Model.value package]
                , Xml.node "bugReports" [] [ToXml.toXml . Package.bugReports $ Model.value package]
                , Xml.node "buildType" [] [ToXml.toXml . Package.buildType $ Model.value package]
                , Xml.node "cabalVersion" [] [ToXml.toXml . Package.cabalVersion $ Model.value package]
                , Xml.node "category" [] [ToXml.toXml . Package.category $ Model.value package]
                , Xml.node "copyright" [] [ToXml.toXml . Package.copyright $ Model.value package]
                , Xml.node "description" [] [ToXml.toXml . parsedDescription $ Model.value package]
                , Xml.node "homepage" [] [ToXml.toXml . Package.homepage $ Model.value package]
                , Xml.node "latest" [] [ToXml.toXml $ isLatest maybeLatestVersion (Package.version $ Model.value package) (Package.revision $ Model.value package)]
                , Xml.node "license" [] [ToXml.toXml . Package.license $ Model.value package]
                , Xml.node "maintainer" [] [ToXml.toXml . Package.maintainer $ Model.value package]
                , Xml.node "name" [] [ToXml.toXml . Package.name $ Model.value package]
                , Xml.node "pkgUrl" [] [ToXml.toXml . Package.pkgUrl $ Model.value package]
                , Xml.node "preferred" [] [ToXml.toXml . isPreferred versionRange $ Model.value package]
                , Xml.node "revision" [] [ToXml.toXml . Package.revision $ Model.value package]
                , Xml.node "stability" [] [ToXml.toXml . Package.stability $ Model.value package]
                , Xml.node "synopsis" [] [ToXml.toXml . Package.synopsis $ Model.value package]
                , Xml.node "uploadedAt" [] [ToXml.toXml . Package.uploadedAt $ Model.value package]
                , Xml.node "uploadedBy" [] [ToXml.toXml . Package.uploadedBy $ Model.value package]
                , Xml.node "version" [] [ToXml.toXml . Package.version $ Model.value package]
                ]
            -- TODO: Move these to a separate page?
            , Xml.node "versions" []
            $ fmap (\ x -> Xml.node "version" []
                [ Xml.node "latest" [] [ToXml.toXml $ isLatest maybeLatestVersion (Package.version x) (Package.revision x)]
                , Xml.node "number" [] [ToXml.toXml $ Package.version x]
                , Xml.node "preferred" [] [ToXml.toXml $ isPreferred versionRange x]
                , Xml.node "revision" [] [ToXml.toXml $ Package.revision x]
                , Xml.node "route" [] [ToXml.toXml $ Route.Release (Package.name x) (Package.release x)]
                , Xml.node "uploadedAt" [] [ToXml.toXml $ Package.uploadedAt x]
                ]) sortedPackages
            -- TODO: Include main library component directly on this page?
            , Xml.node "components" []
            . fmap (\ x -> Xml.node "component" []
                [ Xml.node "tag" [] [ToXml.toXml $ Component.tag x]
                , Xml.node "name" [] [ToXml.toXml $ componentName (Model.value package) x]
                , Xml.node "route" [] [ToXml.toXml $ componentRoute (Model.value package) x]
                ])
            . List.sortOn (\ x -> (Component.tag x, Component.name x))
            $ fmap Model.value components
            , Xml.node "sourceRepositories" []
            . fmap (\ x -> Xml.node "sourceRepository" []
                [ Xml.node "branch" [] [ToXml.toXml $ SourceRepository.branch x]
                , Xml.node "kind" [] [ToXml.toXml $ SourceRepository.kind x]
                , Xml.node "location" [] [ToXml.toXml $ SourceRepository.location x]
                , Xml.node "module" [] [ToXml.toXml $ SourceRepository.module_ x]
                , Xml.node "subdir" [] [ToXml.toXml $ SourceRepository.subdir x]
                , Xml.node "tag" [] [ToXml.toXml $ SourceRepository.tag x]
                , Xml.node "type" [] [ToXml.toXml $ SourceRepository.type_ x]
                ])
            . List.sortOn SourceRepository.location
            $ fmap Model.value sourceRepositories
            -- TODO: Move these to a separate page?
            , Xml.node "files" []
            . Maybe.mapMaybe (\ x -> do
                path <- List.stripPrefix prefix $ File.path x
                pure $ Xml.node "file" []
                    [ Xml.node "path" [] [ToXml.toXml path]
                    , Xml.node "route" [] [ToXml.toXml . Route.File packageName release $ File.path x]
                    ])
            . List.sortOn File.path
            $ fmap Model.value files
            ]
    pure $ Common.makeResponse Root.Root
        { Root.meta = (Meta.fromContext context route)
            { Meta.breadcrumbs = breadcrumbs
            , Meta.title = List.intercalate " - " ["Monadoc", into @String packageName, into @String release]
            , Meta.user = fmap (User.githubLogin . Model.value) maybeUser
            }
        , Root.page = page
        }

componentName :: Package.Package -> Component.Component -> Maybe ComponentName.ComponentName
componentName package component =
    let
        name = Component.name component
        isLibrary = Component.tag component == ComponentTag.Library
        namesMatch = into @PackageName.PackageName name == Package.name package
    in if isLibrary && namesMatch then Nothing else Just name

componentRoute :: Package.Package -> Component.Component -> Route.Route
componentRoute package component =
    let
        name = Package.name package
        release = Package.release package
        tag = Component.tag component
        componentId = ComponentId.ComponentId tag $ componentName package component
    in Route.Component name release componentId

parsedDescription :: Package.Package -> Haddock.DocH Void String
parsedDescription = Haddock.toRegular
    . Haddock._doc
    . Haddock.parseParas Nothing
    . into @String
    . Package.description

isLatest :: Maybe LatestVersion.Model -> Version.Version -> Revision.Revision -> Bool
isLatest m v r = case m of
    Nothing -> False
    Just l -> v == LatestVersion.version (Model.value l)
        && r == LatestVersion.revision (Model.value l)

isPreferred :: VersionRange.VersionRange -> Package.Package -> Bool
isPreferred versionRange package =
    VersionRange.contains (Package.version package) versionRange
