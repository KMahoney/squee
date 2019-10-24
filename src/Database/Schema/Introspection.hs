module Database.Schema.Introspection (introspect) where

import Data.Bifunctor (second)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Text (Text)

import Database.PostgreSQL.Simple (Only(..), Connection, query_)

import Database.Schema.Structure
import Database.Schema.DBType


-- FIXME: only tables, not views, because we can't deterime
-- nullability of view columns.
fetchTables :: Connection -> IO (M.Map TableName Table)
fetchTables connection = do
  columns <- map convertColumn <$> query_ connection sql
  return (M.fromListWith (M.union) (map (second (uncurry M.singleton)) columns))

  where
    sql =
      "SELECT table_name, column_name, udt_name, is_nullable " <>
      "FROM information_schema.columns WHERE table_schema='public'"


convertColumn :: (Text, Text, Text, Text) -> (TableName, (ColumnName, Column))
convertColumn (tableName, columnName, typename, nullable) =
  (TableName tableName, (ColumnName columnName, Column (DBType (nullable == "YES") typename)))
      

fetchSequences :: Connection -> IO (S.Set Text)
fetchSequences connection = (S.fromList . map fromOnly) <$> query_ connection sql

  where
    sql =
      "SELECT sequence_name FROM information_schema.sequences WHERE sequence_schema='public'"


introspect :: Connection -> IO Schema
introspect connection =
  Schema <$> fetchTables connection <*> fetchSequences connection
