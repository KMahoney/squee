module Cli.Command.Repl (run) where

import qualified Data.Map as M
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal
import System.Console.Haskeline
import qualified Data.Text as T
import Database.PostgreSQL.Simple (connectPostgreSQL, Connection)
import Control.Monad.IO.Class
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
import Database.Schema (Schema)
import qualified Database.Schema as Schema


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
stdEnv = M.union Env.stdLib . Env.fromSchema


replLoop :: Connection -> IORef Env -> InputT IO ()
replLoop connection envRef = loop

  where
    loop :: InputT IO ()
    loop = do
      env <- liftIO $ readIORef envRef
      input <- getInputLine "SQUEE> "
      case input of
        Nothing -> return ()
        Just "" ->
          loop
        Just expressionInput ->
          case Parser.parseReplStatement (T.pack expressionInput) of
            Right (AST.RSDefinition def) -> do
              case T.inferDefinition (Env.typeEnv env) def of
                Left err -> do
                  outputDoc (showInferError err)
                  newLine
                  loop
                Right schema -> do
                  newLine
                  outputText $ (AST.symbolName (AST.definitionName def)) <> " : " <> (T.showSchema (T.normaliseTyVars schema))
                  newLine
                  let value = Eval.runEval (Env.valueEnv env) (Eval.evalDefinition def)
                  liftIO $ writeIORef envRef (M.insert (AST.definitionName def) (value, schema) env)
                  loop
            Right (AST.RSExpression ast) -> do
              case T.inferExpression (Env.typeEnv env) ast of
                Left err ->
                  outputDoc (showInferError err)
                Right t ->
                  outputValue t $ Eval.runEval (Env.valueEnv env) (Eval.evalExpression ast)
              newLine
              loop
            Left err -> do
              outputStrLn (T.unpack (Parsec.errMessage err))
              outputDoc $ Parsec.prettyPos $ Parsec.errSourcePos err
              newLine
              loop

    outputText :: T.Text -> InputT IO ()
    outputText = outputStrLn . T.unpack

    outputValue :: T.Qual -> Eval.Value -> InputT IO ()
    outputValue t = \case
      Eval.VQuery query -> do
        newLine
        outputText $ ": " <> (T.showQual (T.normaliseTyVars t))
        newLine
        results <- liftIO $ QB.queryAsText connection query
        outputDoc $ resultsTable query results
      Eval.VSqlExpr e -> do
        newLine
        result <- liftIO $ QB.expressionAsText connection e
        outputStrLn $ "= " <> T.unpack result
      _ -> do
        newLine
        outputText $ ": " <> (T.showQual (T.normaliseTyVars t))

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
