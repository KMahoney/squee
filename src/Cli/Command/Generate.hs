module Cli.Command.Generate where

import Data.Text (Text)
import qualified Data.Map as M
import qualified Data.Text as T

import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal

import Cli.Compile

import qualified Database.Schema as Schema
import qualified Squee.AST as AST
import qualified Squee.Eval as Eval
import qualified Squee.QueryBuilder as QB
import qualified RangedParsec as RP
import qualified Database.Sql as Sql
import qualified Squee.Types.Type as Type
import qualified Squee.Types.PrettyPrint as Type

import RangedParsec (Located(..))


type GeneratorError = Text

type Generator = [CompiledDef] -> Either (Located GeneratorError) Text


toQuery :: Eval.Value -> QB.Query
toQuery = \case
  Eval.VQuery query ->
    query
  Eval.VFn fn ->
    let remainingArgs = Eval.fnArity fn - length (Eval.fnArgs fn)
        values = map (Eval.VSqlExpr . QB.EPlaceholder) [0..toInteger remainingArgs - 1]
        Eval.VQuery query = Eval.evalFn fn values
    in
      query
  _ ->
    -- Case should be prevented by type check
    error "cannot evaluate value to query"


sqlPrepare :: Generator
sqlPrepare defs =
  T.concat <$> mapM prepare defs
  where
    prepare :: CompiledDef -> Either (Located GeneratorError) Text
    prepare (AST.LocalDef _ _ _, _, _) = return ""
    prepare (exportDef, _, value) =
      let query = toQuery value
      in return $ "PREPARE " <> AST.symbolName (AST.definitionName exportDef) <> " AS\n  " <> Sql.toText (toSql query) <> ";\n\n"

    toSql :: QB.Query -> Sql.Sql
    toSql = QB.buildSqlWithPlaceholder preparePlaceholder

    preparePlaceholder :: Integer -> Sql.Sql
    preparePlaceholder i = Sql.Sql $ "$" <> T.pack (show (i + 1))


-- WARNING: extremely hacky code
haskell :: Generator
haskell defs =
  T.intercalate "\n" <$> mapM hsDef defs
  where
    hsDef :: CompiledDef -> Either (Located GeneratorError) Text
    hsDef (AST.LocalDef _ _ _, _, _) = return ""
    hsDef (AST.ExportDef symbol args _, (Type.TypeSchema _ (Type.Qual _ ty)), value) =
      let (argTypes, returnType) = zipFnType args ty
      in return $ template (AST.symbolName symbol) argTypes returnType (toQuery value)

    template :: T.Text -> [(AST.Symbol, Type.Type)] -> Type.Type -> QB.Query -> T.Text
    template name argTypes returnType query =
      name <> " :: Connection -> " <> T.concat (map ((<> " -> ") . hsArgType . snd) argTypes) <> (hsReturnType returnType query) <> "\n" <>
      name <> " connection" <> T.concat (map ((" " <>) .  AST.symbolName . fst) argTypes) <> " = do\n" <>
      "  " <> (if null argTypes then "query_" else "query") <> " connection \"" <> T.replace "\"" "\\\"" (Sql.toText (toSql query)) <> "\" " <>
      (case queryArgs query (map fst argTypes) of
         [] -> ""
         [single] -> "(Only " <> AST.symbolName single <> ")"
         args -> "(" <> T.intercalate ", " (map AST.symbolName args) <> ")"
      ) <> "\n"

    queryArgs :: QB.Query -> [AST.Symbol] -> [AST.Symbol]
    queryArgs query args = map ((args !!) . fromInteger) (QB.collectPlaceholders query)

    toSql :: QB.Query -> Sql.Sql
    toSql = QB.buildSqlWithPlaceholder (const "?")

    -- FIXME: fail gracefully on missing mapping
    hsArgType :: Type.Type -> T.Text
    hsArgType = \case
      Type.TypeCon "~int4" [] -> "Int"
      Type.TypeCon "~text" [] -> "String"
      Type.TypeCon "~bool" [] -> "Bool"
      t -> error ("Unknown Haskell mapping for type " <> T.unpack (Type.showType t))

    hsReturnType :: Type.Type -> QB.Query -> T.Text
    hsReturnType (Type.TypeCon "Query" [Type.TypeCon "Row" [Type.TypeRow typeMap Nothing]]) query =
      case QB.columnNames query of
        [] -> "IO ()"
        [Schema.ColumnName name] -> "IO [Only " <> hsArgType (typeMap M.! (AST.Symbol name)) <> "]"
        ts -> "IO [(" <> T.intercalate ", " (map (\(Schema.ColumnName name) -> hsArgType (typeMap M.! (AST.Symbol name))) ts) <> ")]"
    hsReturnType t _ =
      -- Case should be prevented by type check
      error ("Invalid Haskell return type " <> T.unpack (Type.showType t))

    zipFnType :: [AST.Symbol] -> Type.Type -> ([(AST.Symbol, Type.Type)], Type.Type)
    zipFnType [] ty = ([], ty)
    zipFnType (arg:args) (Type.TypeCon "->" [t1, t2]) =
      let (args', r) = zipFnType args t2 in
        ((arg, t1) : args', r)
    zipFnType _ _ =
      -- Case should be prevented by type check
      error "Expecting -> type"


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
