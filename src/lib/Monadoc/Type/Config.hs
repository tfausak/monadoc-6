module Monadoc.Type.Config where

import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Exception
import qualified Data.String as String
import qualified Monadoc.Exception.InvalidPort as InvalidPort
import qualified Monadoc.Type.Flag as Flag
import qualified Monadoc.Type.Warning as Warning
import qualified Network.Wai.Handler.Warp as Warp
import qualified Text.Read as Read

data Config = Config
    { help :: Bool
    , host :: Warp.HostPreference
    , port :: Warp.Port
    , version :: Bool
    } deriving (Eq, Show)

initial :: Config
initial = Config
    { help = False
    , host = String.fromString "127.0.0.1"
    , port = 3000
    , version = False
    }

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
    Flag.Help -> pure config { help = True }
    Flag.Host x -> pure config { host = String.fromString x }
    Flag.Port string -> case Read.readMaybe string of
        Nothing -> Exception.throwM $ InvalidPort.InvalidPort string
        Just x -> pure config { port = x }
    Flag.Version -> pure config { version = True }
