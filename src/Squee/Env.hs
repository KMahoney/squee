module Squee.Env where

import qualified Data.Map as M

import Database.Schema (Schema)
import qualified Database.Schema as Schema
import qualified Squee.QueryBuilder as QB
import qualified Squee.Types.Type as Type
import qualified Squee.Types.CommonType as Type
import Squee.Eval
import Squee.AST (Symbol(..))
import Squee.StdLib

type Env = M.Map Symbol (Value, Type.TypeSchema)
type TypeEnv = M.Map Symbol Type.TypeSchema


valueEnv :: Env -> ValueEnv
valueEnv = fmap fst


typeEnv :: Env -> TypeEnv
typeEnv = fmap snd


fromSchema :: Schema -> Env
fromSchema schema =
   M.fromList $ map tableEntry $ M.toList $ Schema.schemaTables schema

  where
    tableEntry :: (Schema.TableName, Schema.Table) -> (Symbol, (Value, Type.TypeSchema))
    tableEntry (name, table) =
      (Symbol (Schema.tableName name), (tableValue (name, table), tableType table))
            
    tableValue :: (Schema.TableName, Schema.Table) -> Value
    tableValue (name, table) =
      VQuery $ QB.Query (map (\(Schema.ColumnName col) -> (Schema.ColumnName col, QB.EField col)) (M.keys table)) (QB.SourceTable name) [] Nothing Nothing

    tableType :: Schema.Table -> Type.TypeSchema
    tableType table =
      Type.TypeSchema [] . Type.Qual [] $
      Type.tQuery $ Type.tRow $ Type.TypeRow (M.fromList $ map columnAsRowField $ M.toList table) Nothing

    columnAsRowField :: (Schema.ColumnName, Schema.Column) -> (Symbol, Type.Type)
    columnAsRowField (columnName, column) = (Symbol (Schema.columnName columnName), columnType column)

    columnType :: Schema.Column -> Type.Type
    columnType = flip Type.TypeCon [] . ("~" <>) . Schema.typename . Schema.columnType


stdEnv :: Schema.Schema -> Env
stdEnv = M.union stdLib . fromSchema
