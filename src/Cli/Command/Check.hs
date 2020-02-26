module Cli.Command.Check (run) where

import System.Exit
import qualified Data.Text as T

import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal

import Cli.Compile
import qualified Database.Schema as Schema


putDocLn :: Doc AnsiStyle -> IO ()
putDocLn =
  putStrLn . T.unpack . renderStrict . layoutPretty defaultLayoutOptions


check :: Schema.Schema -> String -> IO Bool
check schema filename = do
  result <- compileSqueeFile schema filename
  case result of
    Left err -> do
      putDocLn (showCompileError err)
      putStrLn ""
      return False
    Right _ ->
      return True


run :: [String] -> IO ()
run filenames = do
  schema <- readSchema
  checks <- mapM (check schema) filenames
  if all id checks
    then exitSuccess
    else exitFailure
