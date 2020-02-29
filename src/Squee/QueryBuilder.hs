module Squee.QueryBuilder
  ( Expression(..)
  , Source(..)
  , Query(..)
  , Settings(..)
  , buildSqlNoPlaceholder
  , buildSqlWithPlaceholder
  , buildSqlExpressionNoPlaceholder
  , buildSqlExpressionWithPlaceholder
  , collectPlaceholders
  , columnNames
  , applyFilter
  , applyOrder
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
import Control.Monad.Reader

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
    , queryOrder :: Maybe Expression
    }

data Settings
  = Settings { placeholderFormat :: Integer -> Sql }

type Build a = Reader Settings a


buildSqlNoPlaceholder :: Query -> Sql
buildSqlNoPlaceholder = buildSqlWithPlaceholder undefined


buildSqlWithPlaceholder :: (Integer -> Sql) -> Query -> Sql
buildSqlWithPlaceholder f query = runReader (toSql query) (Settings f)


buildSqlExpressionNoPlaceholder :: Expression -> Sql
buildSqlExpressionNoPlaceholder = buildSqlExpressionWithPlaceholder undefined


buildSqlExpressionWithPlaceholder :: (Integer -> Sql) -> Expression -> Sql
buildSqlExpressionWithPlaceholder f e = "SELECT " <> runReader (expressionToSql e) (Settings f)


collectPlaceholders :: Query -> [Integer]
collectPlaceholders (Query { columns, querySource, queryJoins, queryFilter }) =
  concatMap colPlaceholders columns ++
  sourcePlaceholders querySource ++
  concatMap sourcePlaceholders queryJoins ++
  maybe [] expressionPlaceholders queryFilter

  where
    colPlaceholders (_, e) = expressionPlaceholders e
    sourcePlaceholders = \case
      SourceTable _ -> []
      SourceQuery query -> collectPlaceholders query
    expressionPlaceholders =
      \case
        EBinOp _ e1 e2 -> expressionPlaceholders e1 ++ expressionPlaceholders e2
        ECast e _ -> expressionPlaceholders e
        EPlaceholder i -> [i]
        _ -> []


columnNames :: Query -> [Schema.ColumnName]
columnNames = map fst . columns


applyFilter :: Expression -> Query -> Query
applyFilter expression query@(Query { queryFilter }) =
  query { queryFilter = maybe (Just expression) (Just . EBinOp "AND" expression) queryFilter }


applyOrder :: Expression -> Query -> Query
applyOrder expression query =
  query { queryOrder = Just expression }


applyMap :: M.Map Text Expression -> Query -> Query
applyMap newColumns query =
  Query { columns = map (\(c, e) -> (Schema.ColumnName c, e)) (M.toList newColumns)
        , querySource = SourceQuery query
        , queryJoins = []
        , queryFilter = Nothing
        , queryOrder = Nothing
        }


applyJoin :: Query -> Query -> Query
applyJoin a b =
  Query { columns = map selectColumn (S.toList (S.fromList (columnNames a) `S.union` S.fromList (columnNames b)))
        , querySource = SourceQuery a
        , queryJoins = [SourceQuery b]
        , queryFilter = Nothing
        , queryOrder = Nothing
        }
  where
    selectColumn (Schema.ColumnName name) = (Schema.ColumnName name, EField name)
  

fieldExpressionToSql :: (Schema.ColumnName, Expression) -> Build Sql
fieldExpressionToSql (Schema.ColumnName columnName, EField f)
  | columnName == f =
    return (quoteName columnName)
fieldExpressionToSql (Schema.ColumnName columnName, e) = do
  e' <- expressionToSql e
  return $ e' <+> "AS" <+> quoteName columnName


expressionToSql :: Expression -> Build Sql
expressionToSql = \case
  EField name ->
    return (quoteName name)
  EBinOp op e1 e2 -> do
    e1' <- expressionToSql e1
    e2' <- expressionToSql e2
    return $ parens e1' <+> Sql op <+> parens e2'
  EString s ->
    return (quoteString s)
  EInt i ->
    return $ Sql $ T.pack $ show i
  ECast e t -> do
    e' <- expressionToSql e
    return $ "(" <> e' <> ")::" <> Sql t
  EPlaceholder i -> do
    f <- asks placeholderFormat
    return (f i)


sourceToSql :: Source -> Build Sql
sourceToSql (SourceTable name) = return $ quoteName $ Schema.tableName name
sourceToSql (SourceQuery query) = parens <$> toSql query


parens :: Sql -> Sql
parens e = "(" <> e <> ")"


toSql :: Query -> Build Sql
toSql (Query { columns, querySource, queryJoins, queryFilter, queryOrder }) = do
  columns' <- mapM fieldExpressionToSql columns
  querySource' <- sourceToSql querySource
  queryJoins' <- joinSql
  queryFilter' <- filterSql
  queryOrder' <- orderSql
  return $
    "SELECT" <+> intercalate "," columns' <+>
    "FROM" <+> querySource' <> " AS _t" <> queryJoins' <> queryFilter' <> queryOrder'
  where
    joinSql =
      mconcat <$>
      mapM (\(i, j) -> do
               source' <- sourceToSql j
               return $
                 " NATURAL JOIN " <> source' <> " AS _j" <> Sql (T.pack (show i))) (zip [(0::Int)..] queryJoins)
    filterSql =
      case queryFilter of
        Nothing -> return ""
        Just e -> (" WHERE" <+>) <$> expressionToSql e
    orderSql =
      case queryOrder of
        Nothing -> return ""
        Just e -> (" ORDER BY" <+>) <$> expressionToSql e


castTextColumns :: Query -> Query
castTextColumns query@(Query { columns }) =
  query { columns = fmap (second (flip ECast "text")) columns }


queryAsText :: PG.Connection -> Query -> IO [[Text]]
queryAsText conn query =
  PG.query_ conn $ toPGQuery $ buildSqlNoPlaceholder $ castTextColumns query


expressionAsText :: PG.Connection -> Expression -> IO Text
expressionAsText conn expression = do
  [[result]] <- PG.query_ conn $ toPGQuery $ buildSqlExpressionNoPlaceholder (ECast expression "text")
  return result

