module Main where

import Options.Applicative
import Control.Monad (join)

import qualified Cli.Command.Repl as Repl
import qualified Cli.Command.Check as Check


options :: Parser (IO ())
options = subparser commands

  where
    commands =
      command "repl" (info (helper <*> replOpts) replDesc) <>
      command "check" (info (helper <*> checkOpts) checkDesc)
      
    replOpts =
      pure Repl.run
      
    replDesc =
      fullDesc <> progDesc "Start REPL"

    checkOpts =
      Check.run <$> many (strArgument (metavar "FILENAME"))
      
    checkDesc =
      fullDesc <> progDesc "Check for errors"


main :: IO ()
main = join (customExecParser optPrefs opts)
  where
    optPrefs = prefs (columns 120 <> showHelpOnEmpty)
    opts = info (helper <*> options) fullDesc
