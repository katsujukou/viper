module Cmd.Install where

import Prelude

import ArgParse.Basic (ArgParser)
import ArgParse.Basic as ArgParser
import Effect.Aff (Aff)
import Effect.Class.Console as Console
import Node.Path as Path

type InstallArgs =
  { cacheDir :: String
  , noCache :: Boolean
  , spagoArgs :: String
  }

argParser :: ArgParser InstallArgs
argParser =
  ArgParser.fromRecord
    { cacheDir:
        ArgParser.argument [ "--cache-dir" ]
          "Path to the directory storing files used by vite.\n\
          \Defaults to './node_modules/.spago'."
          # ArgParser.default (Path.concat [ ".", "node_modules", ".spago" ])
    , noCache:
        ArgParser.flag [ "--no-cache" ]
          "If specified, installing package without storing cache files"
          # ArgParser.boolean
          # ArgParser.default false
    , spagoArgs:
        ArgParser.argument [ "-x", "--spago-args" ]
          "Options given to spago"
          # ArgParser.default ""
    }

cmd :: InstallArgs -> Aff Unit
cmd args = do
  Console.log "Install"