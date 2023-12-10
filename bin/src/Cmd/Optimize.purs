module Cmd.Optimize where

import Prelude

import ArgParse.Basic (ArgParser)
import ArgParse.Basic as ArgParser
import Data.List (List(..))
import Effect.Aff (Aff)
import Effect.Class.Console (logShow)
import Effect.Class.Console as Console
import Viper.Monad (runViperM)
import Viper.Spago (findPackageDescriptor)

type OptimizeArgs = 
  { threshold :: Int
  , exclude :: List String
  }

argParser :: ArgParser OptimizeArgs
argParser = 
  ArgParser.fromRecord
    { threshold:
        ArgParser.argument [ "--threshold", "-T" ]
          "TODO: description of option"
          # ArgParser.int
          # ArgParser.default 5
    , exclude:
        ArgParser.argument [ "--exclude", "-e" ]
          "List of packages which should be excluded from optimized bundle."
          # ArgParser.many
          # ArgParser.default Nil
    }

cmd :: OptimizeArgs -> Aff Unit
cmd args = void $ runViperM { spagoDirectory : "" } do
  res <- findPackageDescriptor "."
  logShow res