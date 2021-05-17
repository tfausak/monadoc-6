module Monadoc.Type.Flag where

import qualified Control.Monad.Catch as Exception
import qualified Data.List.NonEmpty as NonEmpty
import qualified Monadoc.Exception.OptionError as OptionError
import qualified Monadoc.Type.Warning as Warning
import qualified System.Console.GetOpt as Console

data Flag
    = Database String
    | Help
    | Host String
    | Port String
    | Version
    deriving (Eq, Show)

fromArguments :: Exception.MonadThrow m => [String] -> m ([Warning.Warning], [Flag])
fromArguments arguments = do
    let
        (flags, unexpectedArguments, unrecognizedOptions, errorMessages) =
            Console.getOpt' Console.Permute options arguments
        warnings = fmap Warning.UnexpectedArgument unexpectedArguments
            <> fmap Warning.UnrecognizedOption unrecognizedOptions
    case NonEmpty.nonEmpty errorMessages of
        Just xs -> Exception.throwM $ OptionError.OptionError xs
        Nothing -> pure (warnings, flags)

options :: [Console.OptDescr Flag]
options =
    [ Console.Option ['h', '?'] ["help"] (Console.NoArg Help)
        "Outputs this help message to STDOUT and exits successfully."
    , Console.Option [] ["version"] (Console.NoArg Version)
        "Outputs the version number to STDOUT and exits successfully."
    , Console.Option [] ["host"] (Console.ReqArg Host "HOST")
        "Sets the host interface to bind to."
    , Console.Option [] ["port"] (Console.ReqArg Port "PORT")
        "Sets the port number to listen on."
    , Console.Option [] ["database"] (Console.ReqArg Database "DATABASE")
        "Sets the database file to use, or `:memory:` for in-memory."
    ]
