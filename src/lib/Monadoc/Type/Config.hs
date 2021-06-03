module Monadoc.Type.Config where

import Monadoc.Prelude

import qualified Control.Monad as Monad
import qualified Data.List as List
import qualified Monadoc.Type.Flag as Flag
import qualified Monadoc.Type.Port as Port
import qualified Monadoc.Type.Warning as Warning
import qualified Monadoc.Utility.Convert as Convert
import qualified Network.Wai.Handler.Warp as Warp

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
    , port = from @Int 3000
    , version = False
    }

isSecure :: Config -> Bool
isSecure = List.isPrefixOf "https://" <. baseUrl

fromArguments :: MonadThrow m => [String] -> m ([Warning.Warning], Config)
fromArguments arguments = do
    (warnings, flags) <- Flag.fromArguments arguments
    config <- fromFlags flags
    pure (warnings, config)

fromFlags :: (Foldable t, MonadThrow m) => t Flag.Flag -> m Config
fromFlags flags = applyFlags flags initial

applyFlags
    :: (Foldable t, MonadThrow m)
    => t Flag.Flag -> Config -> m Config
applyFlags flags config = Monad.foldM (flip applyFlag) config flags

applyFlag :: MonadThrow m => Flag.Flag -> Config -> m Config
applyFlag flag config = case flag of
    Flag.BaseUrl baseUrl -> pure config { baseUrl }
    Flag.ClientId clientId -> pure config { clientId }
    Flag.ClientSecret clientSecret -> pure config { clientSecret }
    Flag.Database database -> pure config { database }
    Flag.DataDirectory dataDirectory -> pure config { dataDirectory }
    Flag.HackageUrl hackageUrl -> pure config { hackageUrl }
    Flag.Help -> pure config { help = True }
    Flag.Host x -> pure config { host = Convert.stringToHost x }
    Flag.Port string -> do
        port <- either throwM pure <| tryFrom string
        pure config { port }
    Flag.Version -> pure config { version = True }
