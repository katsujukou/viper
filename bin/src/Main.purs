module Main where
  
import Prelude

import ArgParse.Basic (ArgParser)
import ArgParse.Basic as ArgParser
import Cmd.Install as Install
import Cmd.Optimize as Optimize
import Data.Array as Array
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console as Console
import Node.Process as Process
import Version as Version

data Command 
  = Install Install.InstallArgs 
  | Optimize Optimize.OptimizeArgs

argParser :: ArgParser Command
argParser =
  ArgParser.choose "command"
    [ ArgParser.command [ "install" ]
        "Install spago package from registry."
        do
          Install <$> Install.argParser <* ArgParser.flagHelp
    , ArgParser.command [ "optimize" ]
        "Optimize spago packages"
        do
          Optimize <$> Optimize.argParser <* ArgParser.flagHelp
    ]
    <* ArgParser.flagHelp
    <* ArgParser.flagInfo [ "--version", "-v" ] "Show version number." Version.version

parseArgs :: Effect (Either ArgParser.ArgError Command)
parseArgs = do
  cliArgs <- Array.drop 2 <$> Process.argv
  pure $ ArgParser.parseArgs "viper"
    "A PureScript build tool serving as a bridge between spago and vite."
    argParser
    cliArgs

main :: Effect Unit
main =
  parseArgs >>= case _ of
    Left err ->
      Console.error $ ArgParser.printArgError err
    Right cmd -> launchAff_ $ case cmd of
      Install opts -> Install.cmd opts
      Optimize opts -> Optimize.cmd opts