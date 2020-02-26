module Cli.Command.Generate where

import Data.Text (Text)
import qualified Data.Text as T

import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal

import Cli.Compile

import qualified Squee.AST as AST
import qualified Squee.Eval as Eval
import qualified Squee.QueryBuilder as QB
import qualified RangedParsec as RP
import qualified Database.Sql as Sql

import RangedParsec (Located(..))


type GeneratorError = Text

type Generator = [CompiledDef] -> Either (Located GeneratorError) Text


sqlPrepare :: Generator
sqlPrepare defs =
  T.concat <$> mapM prepare defs
  where
    prepare :: CompiledDef -> Either (Located GeneratorError) Text
    prepare (AST.LocalDef _ _ _, _, _) = return ""
    prepare (exportDef, _, Eval.VQuery query) =
      return $ "PREPARE " <> AST.symbolName (AST.definitionName exportDef) <> " AS\n  " <> Sql.toText (QB.toSql query) <> ";\n\n"
    prepare (exportDef@(AST.ExportDef _ args _), _, Eval.VFn fn) =
      let values = map (Eval.VSqlExpr . QB.EPlaceholder) [1..toInteger (length args)]
          Eval.VQuery query = Eval.evalFn fn values
      in
        return $ "PREPARE " <> AST.symbolName (AST.definitionName exportDef) <> " AS\n  " <> Sql.toText (QB.toSql query) <> ";\n\n"
    prepare (def, _, _) =
      error $ "Unexpected value for " <> T.unpack (AST.symbolName (AST.definitionName def))


run :: Generator -> String -> IO ()
run gen filename = do
  schema <- readSchema
  result <- compileSqueeFile schema filename
  case result of
    Left err -> do
      putDoc $ showCompileError err
      putStrLn ""
    Right compiledDefs ->
      case gen compiledDefs of
        Left (At loc genErr) -> do
          putDoc $ pretty genErr <> line <> RP.prettyRange loc <> line
        Right out ->
          putStrLn (T.unpack out)
