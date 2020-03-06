module Squee.StdLib (stdLib) where

import qualified Data.Text as T
import qualified Data.Map as M
import qualified Squee.Types.Type as Type
import qualified Database.Schema as Schema
import qualified Squee.QueryBuilder as QB
import Squee.Types.CommonType
import Squee.AST (Symbol(..))
import Squee.Eval


type EnvEntry = (Value, Type.TypeSchema)


stdLib :: M.Map Symbol EnvEntry
stdLib =
  M.fromList
  [ (Symbol "map", stdMap)
  , (Symbol "filter", stdFilter)
  , (Symbol "order", stdOrder)
  , (Symbol "natjoin", stdNatJoin)
  , (Symbol "join", stdJoin)
  , (Symbol "|", stdPipe)
  , stdBinOp "=" stdEqT
  , stdBinOp "+" stdNumOpT
  , stdBinOp "-" stdNumOpT
  , stdBinOp "*" stdNumOpT
  , stdBinOp "/" stdNumOpT
  , stdBinOp "<" stdCompareT
  , stdBinOp ">" stdCompareT
  ]


-- Helpers

fnValue :: ([Value] -> Value) -> Int -> Value
fnValue f arity = VFn (FnValue f arity [])

tv :: Int -> Type.Type
tv = Type.TypeVar

dbRow :: Int -> Type.Pred
dbRow = Type.ValuesInClass Type.DbValue . tv

schema :: [Int] -> Type.Type -> Type.TypeSchema
schema i = Type.TypeSchema i . Type.Qual []

schemaQual :: [Int] -> [Type.Pred] -> Type.Type -> Type.TypeSchema
schemaQual i q t = Type.TypeSchema i (Type.Qual q t)

queryToRowValue :: QB.Query -> Value
queryToRowValue =
  VRow . M.fromList . map (\(Schema.ColumnName c) -> (Symbol c, VSqlExpr (QB.EField c))) . QB.columnNames

queryToQualRowValue :: Schema.TableName -> QB.Query -> Value
queryToQualRowValue (Schema.TableName table) =
  VRow . M.fromList . map (\(Schema.ColumnName c) -> (Symbol c, VSqlExpr (QB.EQualifiedField table c))) . QB.columnNames

(-->) :: Type.Type -> Type.Type -> Type.Type
(-->) = tFn
infixr -->


-- Filter

stdFilter :: EnvEntry
stdFilter = (fnValue impl 2, ty)
  where
    impl [VFn fn, VQuery q] =
      case fnEval fn [queryToRowValue q] of
        VSqlExpr e ->
          VQuery (QB.applyFilter e q)
        _ ->
          error "expecting sql expression"
    impl _ = undefined
    ty = schemaQual [0] [dbRow 0] $ ((tRow (tv 0)) --> tBool) --> tQuery (tRow (tv 0)) --> tQuery (tRow (tv 0))


-- Order

stdOrder :: EnvEntry
stdOrder = (fnValue impl 2, ty)
  where
    impl [VFn fn, VQuery q] =
      case evalFn fn [queryToRowValue q] of
        VSqlExpr e ->
          VQuery (QB.applyOrder e q)
        _ ->
          error "expecting sql expression"
    impl _ = undefined
    ty = schemaQual [0, 1] [dbRow 0, Type.InClass Type.Comparable (tv 1)] $
      ((tRow (tv 0)) --> (tv 1)) --> tQuery (tRow (tv 0)) --> tQuery (tRow (tv 0))


-- Map

stdMap :: EnvEntry
stdMap = (fnValue impl 2, ty)
  where
    impl [VFn fn, VQuery q] =
      case evalFn fn [queryToRowValue q] of
        VRow rowExprs ->
          let rowExprs' = M.fromList $ map (\(Symbol k, VSqlExpr expr) -> (k, expr)) $ M.toList rowExprs in
            VQuery (QB.applyMap rowExprs' q)
        _ ->
          error "expecting row"
    impl _ = undefined
    ty = schemaQual [0, 1] [dbRow 0, dbRow 1] $ ((tRow (tv 0)) --> tRow (tv 1)) --> tQuery (tRow (tv 0)) --> tQuery (tRow (tv 1))


-- Natural Join

stdNatJoin :: EnvEntry
stdNatJoin = (fnValue impl 2, ty)
  where
    impl [VQuery a, VQuery b] =
      VQuery $ QB.applyNatJoin a b
    impl _ = undefined
    ty = schemaQual [0, 1, 2] [dbRow 0, dbRow 1, dbRow 2, Type.NatJoin (tv 2) (tv 0) (tv 1)] $
      tQuery (tRow (tv 0)) --> tQuery (tRow (tv 1)) --> tQuery (tRow (tv 2))


-- Join

stdJoin :: EnvEntry
stdJoin = (fnValue impl 4, ty)
  where
    impl [VFn condFn, VFn mergeFn, VQuery a, VQuery b] =
      let rowA = queryToQualRowValue (Schema.TableName "_t") a
          rowB = queryToQualRowValue (Schema.TableName "_j0") b
          VSqlExpr cond = fnEval condFn [rowA, rowB]
          VRow merge = fnEval mergeFn [rowA, rowB]
          merge' = M.fromList $ map (\(Symbol k, VSqlExpr expr) -> (k, expr)) $ M.toList merge
      in
        VQuery $ QB.applyJoin cond merge' a b
    impl values = error $ "invalid join application: " <> show values
    rA = tRow (tv 0)
    rB = tRow (tv 1)
    rC = tRow (tv 2)
    qA = tQuery rA
    qB = tQuery rB
    qC = tQuery rC
    ty = schemaQual [0, 1, 2] [dbRow 0, dbRow 1, dbRow 2] $
      (rA --> rB --> tBool) --> (rA --> rB --> rC) --> qA --> qB --> qC


-- Pipe

stdPipe :: EnvEntry
stdPipe = (fnValue impl 2, ty)
  where
    impl [x, VFn fn] = evalFn fn [x]
    impl _ = undefined
    ty = schema [0, 1] $ tv 0 --> (tv 0 --> tv 1) --> tv 1


-- Binary operations

stdBinOp :: T.Text -> Type.TypeSchema -> (Symbol, EnvEntry)
stdBinOp op t =
  (Symbol op, (fnValue (stdBinOpImpl op) 2, t))

stdBinOpImpl :: T.Text -> [Value] -> Value
stdBinOpImpl op [a, b] = case (a, b) of
  (VSqlExpr a', VSqlExpr b') ->
    VSqlExpr (QB.EBinOp op a' b')
  _ ->
    undefined
stdBinOpImpl _ _ = undefined


-- Numeric operations

stdEqT :: Type.TypeSchema
stdEqT = schemaQual [0] [Type.InClass Type.Comparable (tv 0)] $ (tv 0) --> (tv 0) --> tBool

stdNumOpT :: Type.TypeSchema
stdNumOpT = schemaQual [0] [Type.InClass Type.Num (tv 0)] $ (tv 0) --> (tv 0) --> (tv 0)

stdCompareT :: Type.TypeSchema
stdCompareT = schemaQual [0] [Type.InClass Type.Comparable (tv 0)] $ (tv 0) --> (tv 0) --> tBool
