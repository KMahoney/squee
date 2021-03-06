module Cli.Command.Repl (run) where

import qualified Data.Map as M
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal
import System.Console.Haskeline
import qualified Data.Text as T
import Database.PostgreSQL.Simple (connectPostgreSQL, Connection)
import Control.Monad.IO.Class
import Control.Monad (when)
import Data.IORef

import qualified Cli.Pretty.AsciiTable as Table
import Cli.Pretty.InferError
import qualified RangedParsec as Parsec
import qualified Squee.Parser as Parser
import qualified Squee.AST as AST
import qualified Squee.Eval as Eval
import qualified Squee.QueryBuilder as QB
import qualified Squee.Types.PrettyPrint as T
import qualified Squee.Types.Infer as T
import qualified Squee.Types.Type as T
import Squee.AST (symbolName)
import Squee.Env (Env)
import qualified Squee.Env as Env
import Squee.StdLib (stdLib)
import Database.Schema (Schema)
import qualified Database.Sql as Sql
import qualified Database.Schema as Schema


data ReplSettings
  = ReplSettings
  { replShowTypes :: Bool
  , replShowSQL :: Bool
  }


defaultReplSettings :: ReplSettings
defaultReplSettings =
  ReplSettings { replShowTypes = True, replShowSQL = False }


newLine :: InputT IO ()
newLine = outputStrLn ""


renderDoc :: Doc AnsiStyle -> String
renderDoc =
  T.unpack . renderStrict . layoutPretty defaultLayoutOptions


completions :: IORef Env -> CompletionFunc IO
completions envRef (left, _) =
  case span Parser.isIdentifierChar left of
    ([], _) -> return (left, [])
    (identifier, unused) -> do
      env <- readIORef envRef
      let completionList' = filter (T.isPrefixOf (T.pack (reverse identifier))) (map symbolName (M.keys env))
      return (unused, map (simpleCompletion . T.unpack) completionList')
    

stdEnv :: Schema -> Env
stdEnv = M.union stdLib . Env.fromSchema


replLoop :: Connection -> IORef Env -> InputT IO ()
replLoop connection envRef = loop defaultReplSettings

  where
    loop :: ReplSettings -> InputT IO ()
    loop settings = do
      env <- liftIO $ readIORef envRef
      input <- getInputLine "SQUEE> "
      case input of
        Nothing -> return ()
        Just "" ->
          loop settings
        Just ":showsql" ->
          loop (settings { replShowSQL = not (replShowSQL settings) })
        Just ":showtypes" ->
          loop (settings { replShowTypes = not (replShowTypes settings) })
        Just expressionInput ->
          case Parser.parseReplStatement (T.pack expressionInput) of
            Right (AST.RSDefinition def) -> do
              case T.inferDefinition (Env.typeEnv env) def of
                Left err -> do
                  outputDoc (showInferError err)
                  newLine
                  loop settings
                Right schema -> do
                  when (replShowTypes settings) $ do
                    newLine
                    outputText $ (AST.symbolName (AST.definitionName def)) <> " : " <> (T.showSchema (T.normaliseTyVars schema))
                  newLine
                  let value = Eval.runEval (Env.valueEnv env) (Eval.evalDefinition def)
                  liftIO $ writeIORef envRef (M.insert (AST.definitionName def) (value, schema) env)
                  loop settings
            Right (AST.RSExpression ast) -> do
              case T.inferExpression (Env.typeEnv env) ast of
                Left err ->
                  outputDoc (showInferError err)
                Right t ->
                  outputValue settings t $ Eval.runEval (Env.valueEnv env) (Eval.evalExpression ast)
              newLine
              loop settings
            Left err -> do
              outputStrLn (T.unpack (Parsec.errMessage err))
              outputDoc $ Parsec.prettyPos $ Parsec.errSourcePos err
              newLine
              loop settings

    outputText :: T.Text -> InputT IO ()
    outputText = outputStrLn . T.unpack

    outputValue :: ReplSettings -> T.Qual -> Eval.Value -> InputT IO ()
    outputValue settings t = \case
      Eval.VQuery query -> do
        when (replShowTypes settings) $ do
          newLine
          outputText $ ": " <> (T.showQual (T.normaliseTyVars t))
        when (replShowSQL settings) $ do
          newLine
          outputText $ Sql.toText $ QB.buildSqlNoPlaceholder query
        newLine
        results <- liftIO $ QB.queryAsText connection query
        outputDoc $ resultsTable query results
      Eval.VSqlExpr e -> do
        when (replShowSQL settings) $ do
          newLine
          outputText $ Sql.toText $ QB.buildSqlExpressionNoPlaceholder e
        newLine
        result <- liftIO $ QB.expressionAsText connection e
        outputStrLn $ "= " <> T.unpack result
      _ | replShowTypes settings -> do
        newLine
        outputText $ ": " <> (T.showQual (T.normaliseTyVars t))
      _ ->
        newLine

    outputDoc :: Doc AnsiStyle -> InputT IO ()
    outputDoc doc = outputStrLn (renderDoc doc)

    resultsTable :: QB.Query -> [[T.Text]] -> Doc a
    resultsTable query rows =
      let cols = map (\name -> Table.Column (Schema.columnName name) Table.AlignLeft) $ QB.columnNames query in
      Table.render (Table.Table cols rows)
    

run :: IO ()
run = do
  connection <- connectPostgreSQL ""
  schema <- Schema.introspect connection
  envRef <- newIORef (stdEnv schema)
  runInputT (setComplete (completions envRef) defaultSettings) (replLoop connection envRef)
