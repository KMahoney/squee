module Cli.Command.Check (run) where

import qualified Data.Map as M
import qualified Data.Text as T
import Data.List (intercalate)

import Database.PostgreSQL.Simple (connectPostgreSQL)
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal

import qualified Squee.Types.Infer as Type
import qualified Squee.Types.Unify as Type
import qualified Squee.Types.PrettyPrint as Type

import Squee.Parser
import qualified Squee.Env as Env
import qualified Squee.AST as AST
import qualified RangedParsec as RP
import RangedParsec (Located(..))
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
          putInferError err
          putStrLn ""
        Right _ ->
          return ()

  where
    putInferError :: Type.InferError -> IO ()
    putInferError = \case
      Type.InferUnknown (At errSpan _) -> do
        putStrLn "Unknown variable"
        putDocLn $ RP.prettyRange errSpan
      Type.InferUnificationError (At errSpan (Type.UnificationError a b)) -> do
        putStrLn $ "Cannot unify " <> (T.unpack (Type.showType a)) <> " with " <> (T.unpack (Type.showType b))
        putDocLn $ RP.prettyRange errSpan
      Type.InferUnificationError (At errSpan (Type.MissingFields fields)) -> do
        putStrLn $ "Missing fields " <> intercalate ", " (map (T.unpack . AST.symbolName) fields)
        putDocLn $ RP.prettyRange errSpan
      Type.InferPredViolation (At errSpan preds) -> do
        putStrLn $ "Violates constraint(s) " <> intercalate " " (map (T.unpack . Type.showPred) preds)
        putDocLn $ RP.prettyRange errSpan
    
  

run :: [String] -> IO ()
run filenames = do
  connection <- connectPostgreSQL ""
  schema <- Schema.introspect connection
  let env = stdEnv schema
  mapM_ (check env) filenames
