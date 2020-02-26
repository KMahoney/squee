module Squee.Env where

import qualified Data.Map as M
import Data.Text (Text)

import Database.Schema (Schema)
import qualified Database.Schema as Schema
import qualified Squee.QueryBuilder as QB
import Squee.Types.Type (TypeSchema(..), Type(..), Qual(..), Pred(..), TypeClass(..))
import qualified Squee.Types.CommonType as T
import Squee.Eval
import Squee.AST (Symbol(..))


type Env = M.Map Symbol (Value, TypeSchema)
type TypeEnv = M.Map Symbol TypeSchema


valueEnv :: Env -> ValueEnv
valueEnv = fmap fst


typeEnv :: Env -> TypeEnv
typeEnv = fmap snd


stdLib :: Env
stdLib =
  M.fromList
  [ (Symbol "map", (vfn stdMap 2, stdMapT))
  , (Symbol "filter", (vfn stdFilter 2, stdFilterT))
  , (Symbol "natjoin", (vfn stdNatJoin 2, stdNatJoinT))
  , (Symbol "|", (vfn stdPipe 2, stdPipeT))
  , stdBinOp "=" stdEqT
  , stdBinOp "+" stdNumOpT
  , stdBinOp "-" stdNumOpT
  , stdBinOp "*" stdNumOpT
  , stdBinOp "/" stdNumOpT
  , stdBinOp "<" stdCompareT
  , stdBinOp ">" stdCompareT
  ]

  where
    vfn f arity = VFn (FnValue f arity [])

    fnT = T.tFn
    row = T.tRow
    query = T.tQuery
    bool = T.tBool
    tv = TypeVar
    s i = TypeSchema i . Qual []
    sq i q t = TypeSchema i (Qual q t)
    (-->) = fnT
    infixr -->
    
    stdFilter [VFn (FnValue eval _ args), VQuery q] = do
      let rowValue = M.fromList $ map (\(Schema.ColumnName c) -> (Symbol c, VSqlExpr (QB.EField c))) $ QB.columnNames q
      result <- eval (reverse (VRow rowValue : args))
      case result of
        VSqlExpr e ->
          return $ VQuery (QB.applyFilter e q)
        _ ->
          error "expecting sql expression"
    stdFilter _ = undefined
    stdFilterT = s [0] $ ((row (tv 0)) --> bool) --> query (row (tv 0)) --> query (row (tv 0))

    stdMap [VFn (FnValue eval _ args), VQuery q] = do
      let rowValue = M.fromList $ map (\(Schema.ColumnName c) -> (Symbol c, VSqlExpr (QB.EField c))) $ QB.columnNames q
      result <- eval (reverse (VRow rowValue : args))
      case result of
        VRow rowExprs ->
          let rowExprs' = M.fromList $ map (\(Symbol k, VSqlExpr expr) -> (k, expr)) $ M.toList rowExprs in
            return $ VQuery (QB.applyMap rowExprs' q)
        _ ->
          error "expecting row"
    stdMap _ = undefined
    stdMapT = s [0, 1] $ ((row (tv 0)) --> row (tv 1)) --> query (row (tv 0)) --> query (row (tv 1))

    stdNatJoin [VQuery a, VQuery b] =
      return $ VQuery $ QB.applyJoin a b
    stdNatJoin _ = undefined
    stdNatJoinT = sq [0, 1, 2] [NatJoin (tv 2) (tv 0) (tv 1)] $ query (row (tv 0)) --> query (row (tv 1)) --> query (row (tv 2))

    stdPipe [x, VFn (FnValue eval _ args)] = eval (reverse (x : args))
    stdPipe _ = undefined
    stdPipeT = s [0, 1] $ tv 0 --> (tv 0 --> tv 1) --> tv 1

    stdBinOp :: Text -> TypeSchema -> (Symbol, (Value, TypeSchema))
    stdBinOp op t =
      (Symbol op, (vfn (stdBinOpImpl op) 2, t))

    stdBinOpImpl :: Text -> [Value] -> Eval Value
    stdBinOpImpl op [a, b] = case (a, b) of
      (VSqlExpr a', VSqlExpr b') ->
        return $ VSqlExpr (QB.EBinOp op a' b')
      _ ->
        undefined
    stdBinOpImpl _ _ = undefined

    stdEqT = sq [0] [InClass Comparable (tv 0)] $ (tv 0) --> (tv 0) --> bool
    stdNumOpT = sq [0] [InClass Num (tv 0)] $ (tv 0) --> (tv 0) --> (tv 0)
    stdCompareT = sq [0] [InClass Comparable (tv 0)] $ (tv 0) --> (tv 0) --> bool


fromSchema :: Schema -> Env
fromSchema schema =
   M.fromList $ map tableEntry $ M.toList $ Schema.schemaTables schema

  where
    tableEntry :: (Schema.TableName, Schema.Table) -> (Symbol, (Value, TypeSchema))
    tableEntry (name, table) =
      (Symbol (Schema.tableName name), (tableValue (name, table), tableType table))
            
    tableValue :: (Schema.TableName, Schema.Table) -> Value
    tableValue (name, table) =
      VQuery $ QB.Query (map (\(Schema.ColumnName col) -> (Schema.ColumnName col, QB.EField col)) (M.keys table)) (QB.SourceTable name) [] Nothing

    tableType :: Schema.Table -> TypeSchema
    tableType table =
      TypeSchema [] . Qual [] $
      TypeCon "Query" [TypeCon "Row" [TypeRow (M.fromList $ map columnAsRowField $ M.toList table) Nothing]]

    columnAsRowField :: (Schema.ColumnName, Schema.Column) -> (Symbol, Type)
    columnAsRowField (columnName, column) = (Symbol (Schema.columnName columnName), columnType column)

    columnType :: Schema.Column -> Type
    columnType = flip TypeCon [] . ("~" <>) . Schema.typename . Schema.columnType


stdEnv :: Schema.Schema -> Env
stdEnv = M.union stdLib . fromSchema
