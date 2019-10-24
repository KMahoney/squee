module Squee.Types.Constraints where

import Control.Monad.State

import qualified Data.Map as M
import qualified Squee.Types.Type as T
import qualified Squee.Types.CommonType as T
import qualified Squee.AST as AST
import Squee.Types.Monad.TypeCheck
import RangedParsec (Located(..))

data Constraint
  = ConsEq T.Type T.Type

type Assumption = (Located AST.Symbol, T.Type)


assumptionSymbol :: Assumption -> AST.Symbol
assumptionSymbol (At _ sym, _) = sym


asLookup :: AST.Symbol -> [Assumption] -> [Located T.Type]
asLookup sym = map (\(At loc _, t) -> At loc t) . filter ((== sym) . assumptionSymbol)


asRemove :: [AST.Symbol] -> [Assumption] -> [Assumption]
asRemove syms = filter (not . (`elem` syms) . assumptionSymbol)


generate :: AST.Expression -> TypeCheck err ([Assumption], [Located Constraint], T.Type)
generate =
  \case
    AST.Var locSymbol -> do
      t <- freshVar
      return ([(locSymbol, t)], [], t)
    AST.App e1 (At argSpan e2) -> do
      (as1, c1, t1) <- generate e1
      (as2, c2, t2) <- generate e2
      v <- freshVar
      let c = At argSpan $ ConsEq t1 (t2 `T.tFn` v)
      return (as1 ++ as2, c : c1 ++ c2, v)
    AST.Abs args e -> do
      (as, c, t) <- generate e
      argTypes <- replicateM (length args) freshVar
      let args' = zip args argTypes
          argEq argT (At loc argT') = At loc (ConsEq argT argT')
          c' = concat $ map (\(arg, argT) -> map (argEq argT) (asLookup arg as)) args'
      return (asRemove args as, c ++ c', foldr T.tFn t argTypes)
    AST.Field e (At fieldSpan fieldName) -> do
      (as, c, t) <- generate e
      v <- freshVar
      rowVar <- freshVarInt
      let c' = At fieldSpan (ConsEq t (T.tRow (T.TypeRow (M.singleton fieldName v) (Just rowVar))))
      return (as, c' : c, v)
    AST.BinOp op e1 e2 -> do
      (as1, c1, t1) <- generate e1
      (as2, c2, t2) <- generate e2
      v <- freshVar
      opT <- freshVar
      let a = (op, opT)
      let c = At (locatedSpan op) $ ConsEq opT (t1 `T.tFn` (t2 `T.tFn` v))
      return (a : as1 ++ as2, c : c1 ++ c2, v)
    AST.Lit lit ->
      case lit of
        AST.LitString _ -> return ([], [], T.tText)
        AST.LitInt _ -> return ([], [], T.tInt4)
        AST.LitRow fields -> do
          (as, c, t) <- foldM (\(as, c, rowT) (name, expr) -> do
                                  (as', c', exprT) <- generate expr
                                  return (as ++ as', c ++ c', (name, exprT) : rowT))
                          ([], [], [])
                          fields
          return (as, c, T.tRowList t)
