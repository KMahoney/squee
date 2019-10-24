module Squee.Eval where

import qualified Data.Map as M
import Control.Monad.Reader
import qualified Data.Text as T

import qualified Squee.QueryBuilder as QB
import Squee.AST
import RangedParsec.Pos


type ValueEnv = M.Map Symbol Value

type Eval a = Reader ValueEnv a

data FnValue
  = FnValue { fnEval :: [Value] -> Eval Value
            , fnArity :: Int
            , fnArgs :: [Value]
            }

data Value
  = VQuery QB.Query
  | VFn FnValue
  | VRow ValueEnv
  | VSqlExpr QB.Expression


envLookup :: Located Symbol -> Eval Value
envLookup (At _ name) = do
  env <- ask
  case M.lookup name env of
    Just value -> return value
    Nothing -> error ("unknown var: " <> T.unpack (symbolName name))


runEval :: ValueEnv -> Eval a -> a
runEval env eval =
  runReader eval env


evalAbs :: [Symbol] -> Expression -> [Value] -> Eval Value
evalAbs args ast values =
  withReaderT (M.union (M.fromList (zip args values))) (evalExpression ast)


evalExpression :: Expression -> Eval Value
evalExpression = \case
  Var name ->
    envLookup name

  App e1 (At _ e2) -> do
    v1 <- evalExpression e1
    case v1 of
      VFn fn@(FnValue { fnEval, fnArity, fnArgs }) -> do
        v2 <- evalExpression e2
        let args = v2 : fnArgs in
          if length args == fnArity
          then
            fnEval (reverse args)
          else
            return $ VFn $ fn { fnArgs = args }
      _ ->
        error "left side of application should evaluate to a function"

  Abs args ast ->
    return $ VFn $ FnValue (evalAbs args ast) (length args) []

  Field e (At _ name) -> do
    v1 <- evalExpression e
    case v1 of
      VRow m ->
        case M.lookup name m of
          Just v2 ->
            return v2
          Nothing ->
            error ("missing field: " <> T.unpack (symbolName name))
      _ ->
        error "left side of field access should evaluate to a row"

  BinOp op e1 e2 -> do
    opFn <- envLookup op
    v1 <- evalExpression e1
    v2 <- evalExpression e2
    case opFn of
      VFn (FnValue { fnEval, fnArity = 2, fnArgs = [] }) ->
        fnEval [v1, v2]
      _ ->
        error "operator should evaluate to a function"

  Lit lit ->
    case lit of
      LitString s -> return $ VSqlExpr $ QB.EString s
      LitInt i -> return $ VSqlExpr $ QB.EInt i
      LitRow fields ->
        VRow . M.fromList <$> mapM (\(field, e) -> (field,) <$> evalExpression e) fields
