module Cli.Read where

import qualified Data.Text as T

import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal

import Database.PostgreSQL.Simple (connectPostgreSQL)

import qualified Squee.Types.Infer as Type
import qualified Squee.Types.Type as Type

import Cli.Pretty.InferError
import Squee.Parser
import qualified Squee.AST as AST
import qualified Squee.Env as Env
import qualified RangedParsec as RP
import qualified Database.Schema as Schema


data ReadError
  = ReadInferError Type.InferError
  | ReadParseError RP.ParseError


showReadError :: ReadError -> Doc AnsiStyle
showReadError = \case
  ReadParseError err ->
    pretty (RP.errMessage err) <> line <> RP.prettyPos (RP.errSourcePos err)
  ReadInferError err ->
    showInferError err


readSchema :: IO Schema.Schema
readSchema = do
  connection <- connectPostgreSQL ""
  Schema.introspect connection


readSqueeFile :: Schema.Schema -> String -> IO (Either ReadError [(AST.Definition, Type.TypeSchema)])
readSqueeFile schema filename = do
  source <- readFile filename
  case parseDefinitions (T.pack filename) (T.pack source) of
    Left err -> return (Left (ReadParseError err))
    Right ast ->
      case Type.inferDefinitions (Env.typeEnv (Env.stdEnv schema)) ast of
        Left err -> return (Left (ReadInferError err))
        Right t -> return (Right t)
