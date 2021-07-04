module Monadoc.Data.Migrations where

import Monadoc.Prelude

import qualified Data.List as List
import qualified Monadoc.Model.Blob as Blob
import qualified Monadoc.Model.Component as Component
import qualified Monadoc.Model.Dependency as Dependency
import qualified Monadoc.Model.Distribution as Distribution
import qualified Monadoc.Model.File as File
import qualified Monadoc.Model.HackageIndex as HackageIndex
import qualified Monadoc.Model.HackageUser as HackageUser
import qualified Monadoc.Model.LatestVersion as LatestVersion
import qualified Monadoc.Model.Migration as Migration
import qualified Monadoc.Model.Module as Module
import qualified Monadoc.Model.Package as Package
import qualified Monadoc.Model.PreferredVersions as PreferredVersions
import qualified Monadoc.Model.Session as Session
import qualified Monadoc.Model.SourceRepository as SourceRepository
import qualified Monadoc.Model.User as User

migrations :: [Migration.Migration]
migrations = List.sortOn Migration.time $ mconcat
    [ Blob.migrations
    , Component.migrations
    , Dependency.migrations
    , Distribution.migrations
    , File.migrations
    , HackageIndex.migrations
    , HackageUser.migrations
    , LatestVersion.migrations
    , Module.migrations
    , Package.migrations
    , PreferredVersions.migrations
    , Session.migrations
    , SourceRepository.migrations
    , User.migrations
    ]
