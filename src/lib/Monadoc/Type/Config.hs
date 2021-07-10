{-# LANGUAGE TypeApplications #-}

module Monadoc.Type.Config where

import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Exception
import qualified Data.List as List
import qualified Monadoc.Type.Flag as Flag
import qualified Monadoc.Type.Port as Port
import qualified Monadoc.Type.Warning as Warning
import qualified Monadoc.Utility.Convert as Convert
import qualified Network.Wai.Handler.Warp as Warp
import qualified Witch

data Config = Config
    { baseUrl :: String
    , clientId :: String
    , clientSecret :: String
    , database :: String
    , dataDirectory :: FilePath
    , hackageUrl :: String
    , help :: Bool
    , host :: Warp.HostPreference
    , port :: Port.Port
    , version :: Bool
    } deriving (Eq, Show)

initial :: Config
initial = Config
    { baseUrl = "http://localhost:3000"
    , clientId = "235ce8c873f4ed90905c"
    , clientSecret = "48e202a2b3aa30ad2a4e844f77b7d10807ab1deb"
    , database = "monadoc.sqlite3"
    , dataDirectory = "./data"
    , hackageUrl = "https://hackage.haskell.org"
    , help = False
    , host = Convert.stringToHost "127.0.0.1"
    , port = Witch.from @Int 3000
    , version = False
    }

isSecure :: Config -> Bool
isSecure = List.isPrefixOf "https://" . baseUrl

fromArguments :: Exception.MonadThrow m => [String] -> m ([Warning.Warning], Config)
fromArguments arguments = do
    (warnings, flags) <- Flag.fromArguments arguments
    config <- fromFlags flags
    pure (warnings, config)

fromFlags :: (Foldable t, Exception.MonadThrow m) => t Flag.Flag -> m Config
fromFlags flags = applyFlags flags initial

applyFlags
    :: (Foldable t, Exception.MonadThrow m)
    => t Flag.Flag -> Config -> m Config
applyFlags flags config = Monad.foldM (flip applyFlag) config flags

applyFlag :: Exception.MonadThrow m => Flag.Flag -> Config -> m Config
applyFlag flag config = case flag of
    Flag.BaseUrl bu -> pure config { baseUrl = bu }
    Flag.ClientId ci -> pure config { clientId = ci }
    Flag.ClientSecret cs -> pure config { clientSecret = cs }
    Flag.Database d -> pure config { database = d }
    Flag.DataDirectory dd -> pure config { dataDirectory = dd }
    Flag.HackageUrl hu -> pure config { hackageUrl = hu }
    Flag.Help -> pure config { help = True }
    Flag.Host h -> pure config { host = Convert.stringToHost h }
    Flag.Port string -> do
        p <- either Exception.throwM pure $ Witch.tryFrom string
        pure config { port = p }
    Flag.Version -> pure config { version = True }
