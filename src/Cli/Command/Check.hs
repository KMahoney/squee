module Cli.Command.Check (run) where

import qualified Data.Map as M
import qualified Data.Text as T

import Database.PostgreSQL.Simple (connectPostgreSQL)
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal

import Cli.Pretty.InferError

import qualified Squee.Types.Infer as Type

import Squee.Parser
import qualified Squee.Env as Env
import qualified RangedParsec as RP
import qualified Database.Schema as Schema


putDocLn :: Doc AnsiStyle -> IO ()
putDocLn =
  putStrLn . T.unpack . renderStrict . layoutPretty defaultLayoutOptions


stdEnv :: Schema.Schema -> Env.Env
stdEnv = M.union Env.stdLib . Env.fromSchema


check :: Env.Env -> String -> IO ()
check env filename = do
  source <- readFile filename
  case parseDefinitions (T.pack filename) (T.pack source) of
    Left err -> do
      putStrLn (T.unpack (RP.errMessage err))
      putDocLn $ RP.prettyPos $ RP.errSourcePos err
    Right ast -> do
      case Type.inferDefinitions (Env.typeEnv env) ast of
        Left err -> do
          putDocLn (showInferError err)
          putStrLn ""
        Right _ ->
          return ()


run :: [String] -> IO ()
run filenames = do
  connection <- connectPostgreSQL ""
  schema <- Schema.introspect connection
  let env = stdEnv schema
  mapM_ (check env) filenames
