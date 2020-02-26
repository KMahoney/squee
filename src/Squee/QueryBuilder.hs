module Squee.QueryBuilder
  ( Expression(..)
  , Source(..)
  , Query(..)
  , columnNames
  , applyFilter
  , applyMap
  , applyJoin
  , toSql
  , queryAsText
  , expressionAsText
  ) where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Text (Text)
import Control.Arrow (second)

import Database.Sql
import qualified Database.Schema as Schema
import qualified Database.PostgreSQL.Simple as PG


data Expression
  = EField Text
  | EBinOp Text Expression Expression
  | EString Text
  | EInt Integer
  | ECast Expression Text
  | EPlaceholder Integer

data Source
  = SourceTable Schema.TableName
  | SourceQuery Query

data Query
  = Query
    { columns :: [(Schema.ColumnName, Expression)]
    , querySource :: Source
    , queryJoins :: [Source]
    , queryFilter :: Maybe Expression
    }


columnNames :: Query -> [Schema.ColumnName]
columnNames = map fst . columns


applyFilter :: Expression -> Query -> Query
applyFilter expression query@(Query { queryFilter }) =
  query { queryFilter = maybe (Just expression) (Just . EBinOp "AND" expression) queryFilter }


applyMap :: M.Map Text Expression -> Query -> Query
applyMap newColumns query =
  Query { columns = map (\(c, e) -> (Schema.ColumnName c, e)) (M.toList newColumns)
        , querySource = SourceQuery query
        , queryJoins = []
        , queryFilter = Nothing
        }


applyJoin :: Query -> Query -> Query
applyJoin a b =
  Query { columns = map selectColumn (S.toList (S.fromList (columnNames a) `S.union` S.fromList (columnNames b)))
        , querySource = SourceQuery a
        , queryJoins = [SourceQuery b]
        , queryFilter = Nothing
        }
  where
    selectColumn (Schema.ColumnName name) = (Schema.ColumnName name, EField name)
  

fieldExpressionToSql :: (Schema.ColumnName, Expression) -> Sql
fieldExpressionToSql (Schema.ColumnName columnName, EField f)
  | columnName == f = quoteName columnName
fieldExpressionToSql (Schema.ColumnName columnName, e)
  = expressionToSql e <+> "AS" <+> quoteName columnName


expressionToSql :: Expression -> Sql
expressionToSql = \case
  EField name -> quoteName name
  EBinOp op e1 e2 -> parens (expressionToSql e1) <+> Sql op <+> parens (expressionToSql e2)
  EString s -> quoteString s
  EInt i -> Sql $ T.pack $ show i
  ECast e t -> "(" <> expressionToSql e <> ")::" <> Sql t
  EPlaceholder i -> "$" <> (Sql $ T.pack $ show i)


sourceToSql :: Source -> Sql
sourceToSql (SourceTable name) = quoteName (Schema.tableName name)
sourceToSql (SourceQuery query) = parens (toSql query)


parens :: Sql -> Sql
parens e = "(" <> e <> ")"


toSql :: Query -> Sql
toSql (Query { columns, querySource, queryJoins, queryFilter }) =
  "SELECT" <+> intercalate "," (map fieldExpressionToSql columns) <+>
  "FROM" <+> sourceToSql querySource <> " AS x" <> joinSql <> filterSql
  where
    joinSql =
      mconcat $ map (\(i, j) -> " NATURAL JOIN " <> sourceToSql j <> " AS j" <> Sql (T.pack (show i))) (zip [(0::Int)..] queryJoins)
    filterSql =
      case queryFilter of
        Nothing -> ""
        Just e -> " WHERE" <+> expressionToSql e


castTextColumns :: Query -> Query
castTextColumns query@(Query { columns }) =
  query { columns = fmap (second (flip ECast "text")) columns }


queryAsText :: PG.Connection -> Query -> IO [[Text]]
queryAsText conn query =
  PG.query_ conn (toPGQuery (toSql (castTextColumns query)))


expressionAsText :: PG.Connection -> Expression -> IO Text
expressionAsText conn expression = do
  let sql = "SELECT (" <> expressionToSql expression <> ")::text"
  [[result]] <- PG.query_ conn (toPGQuery sql)
  return result
