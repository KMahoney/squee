module Cli.Command.Repl (run) where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal
import System.Console.Haskeline
import qualified Data.Text as T
import Database.PostgreSQL.Simple (connectPostgreSQL, Connection)
import Control.Monad.IO.Class
import Data.List (intercalate)
import Data.IORef

import qualified Cli.AsciiTable as Table
import qualified RangedParsec as Parsec
import qualified Squee.Parser as Parser
import qualified Squee.AST as AST
import qualified Squee.Eval as Eval
import qualified Squee.QueryBuilder as QB
import qualified Squee.Types.PrettyPrint as T
import qualified Squee.Types.Infer as T
import qualified Squee.Types.Unify as T
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
            Right (AST.RSAssignment sym ast) -> do
              case T.infer (Env.typeEnv env) ast of
                Left err -> do
                  outputInferError err
                  newLine
                  loop
                Right t -> do
                  newLine
                  outputText $ (AST.symbolName sym) <> " : " <> (T.showQual (T.normaliseTyVars t))
                  newLine
                  let schema = T.generalise S.empty t
                      value = Eval.runEval (Env.valueEnv env) (Eval.evalExpression ast)
                  liftIO $ writeIORef envRef (M.insert sym (value, schema) env)
                  loop 
            Right (AST.RSExpression ast) -> do
              case T.infer (Env.typeEnv env) ast of
                Left err ->
                  outputInferError err
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

    outputInferError :: T.InferError -> InputT IO ()
    outputInferError = \case
      T.InferUnknown (Parsec.At errSpan _) -> do
        outputStrLn "Unknown variable"
        outputDoc $ Parsec.prettyRange errSpan
      T.InferUnificationError (Parsec.At errSpan (T.UnificationError a b)) -> do
        outputStrLn $ "Cannot unify " <> (T.unpack (T.showType a)) <> " with " <> (T.unpack (T.showType b))
        outputDoc $ Parsec.prettyRange errSpan
      T.InferUnificationError (Parsec.At errSpan (T.MissingFields fields)) -> do
        outputStrLn $ "Missing fields " <> intercalate ", " (map (T.unpack . symbolName) fields)
        outputDoc $ Parsec.prettyRange errSpan
      T.InferPredViolation (Parsec.At errSpan preds) -> do
        outputStrLn $ "Violates constraint(s) " <> intercalate " " (map (T.unpack . T.showPred) preds)
        outputDoc $ Parsec.prettyRange errSpan

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
