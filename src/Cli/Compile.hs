module Cli.Compile where

import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.Bifunctor as Bi

import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal

import Database.PostgreSQL.Simple (connectPostgreSQL)

import qualified Squee.Types.Infer as Type
import qualified Squee.Types.Type as Type

import Cli.Pretty.InferError
import Squee.Parser
import qualified Squee.AST as AST
import qualified Squee.Env as Env
import qualified Squee.Eval as Eval
import qualified RangedParsec as RP
import qualified Database.Schema as Schema


type CompiledDef = (AST.Definition, Type.TypeSchema, Eval.Value)

data CompileError
  = CompileInferError Type.InferError
  | CompileParseError RP.ParseError


showCompileError :: CompileError -> Doc AnsiStyle
showCompileError = \case
  CompileParseError err ->
    pretty (RP.errMessage err) <> line <> RP.prettyPos (RP.errSourcePos err)
  CompileInferError err ->
    showInferError err


readSchema :: IO Schema.Schema
readSchema = do
  connection <- connectPostgreSQL ""
  Schema.introspect connection


compileSqueeFile :: Schema.Schema -> String -> IO (Either CompileError [CompiledDef])
compileSqueeFile schema filename = do
  source <- readFile filename
  case parseDefinitions (T.pack filename) (T.pack source) of
    Left err -> return $ Left (CompileParseError err)
    Right defs -> return $ compileDefinitions (Env.stdEnv schema) defs


compileDefinitions :: Env.Env -> [AST.Definition] -> Either CompileError [CompiledDef]
compileDefinitions _ [] = return []
compileDefinitions env (def:defs) = do
  typeSchema <- Bi.first CompileInferError (Type.inferDefinition (Env.typeEnv env) def)
  let value = Eval.runEval (Env.valueEnv env) (Eval.evalDefinition def)
      env' = M.insert (AST.definitionName def) (value, typeSchema) env
  ((def, typeSchema, value):) <$> compileDefinitions env' defs
