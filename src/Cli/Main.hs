module Main where

import Options.Applicative
import Control.Monad (join)

import qualified Cli.Command.Repl as Repl
import qualified Cli.Command.Check as Check
import qualified Cli.Command.Generate as Generate


options :: Parser (IO ())
options = hsubparser commands

  where
    commands =
      command "repl" (info replOpts replDesc) <>
      command "check" (info checkOpts checkDesc) <>
      command "generate" (info (hsubparser generators) generateDesc)

    generators =
      command "sql-prepare" (info (generateOpts Generate.sqlPrepare) generateSqlPrepareDesc) <>
      command "hs-postgresql-simple" (info (generateOpts Generate.haskell) generateHaskellDesc)
      
    replOpts =
      pure Repl.run
      
    replDesc =
      fullDesc <> progDesc "Start REPL"

    checkOpts =
      Check.run <$> many (strArgument (metavar "FILENAME"))
      
    checkDesc =
      fullDesc <> progDesc "Check for errors"

    generateOpts generator =
      Generate.run generator <$> strArgument (metavar "FILENAME")

    generateDesc =
      fullDesc <> progDesc "Generate code from export statements"

    generateSqlPrepareDesc =
      fullDesc <> progDesc "Generate SQL PREPARE statements"

    generateHaskellDesc =
      fullDesc <> progDesc "Generate Haskell postgresql-simple definitions"


main :: IO ()
main = join (customExecParser optPrefs opts)
  where
    optPrefs = prefs (columns 120 <> showHelpOnEmpty)
    opts = info (helper <*> options) fullDesc
