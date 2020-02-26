module Squee.Types.Infer
  ( InferError(..)
  , errorPos
  , inferExpression
  , inferDefinition
  ) where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import Control.Monad.Except
import Data.List (nub)

import RangedParsec (Located(..), SourceSpan, SourcePos, firstSourcePos)

import qualified Squee.AST as AST
import qualified Squee.Env as Env
import qualified Squee.Types.Type as T
import qualified Squee.Types.Unify as T
import qualified Squee.Types.Constraints as Constraints
import Squee.Types.Monad.TypeCheck

data InferError
  = InferUnificationError (Located T.UnificationError)
  | InferPredViolation (Located [T.Pred])
  | InferUnknown (Located AST.Symbol)
  deriving (Eq, Show)


errorPos :: InferError -> SourcePos
errorPos = \case
  InferUnificationError (At loc _) -> firstSourcePos loc
  InferPredViolation (At loc _) -> firstSourcePos loc
  InferUnknown (At loc _) -> firstSourcePos loc


instantiateAll :: Env.TypeEnv -> [Constraints.Assumption] -> TypeCheck InferError [T.Pred]
instantiateAll env assumps = nub . concat <$> mapM instantiateAssumption assumps
  where
    instantiateAssumption (locSymbol@(At loc symbol), t) =
      case M.lookup symbol env of
        Just ts -> do
          T.Qual preds t' <- instantiate ts
          mapError (InferUnificationError . At loc) (T.unify t t')
          return preds
        Nothing -> throwError (InferUnknown locSymbol)


unify :: SourceSpan -> T.Type -> T.Type -> TypeCheck InferError ()
unify loc a b = mapError (InferUnificationError . At loc) (T.unify a b)


reducePreds :: SourceSpan -> [T.Pred] -> TypeCheck InferError [T.Pred]
reducePreds _ [] = return []
reducePreds loc (p:ps) = do
  p' <- updateType p
  case p' of
    (T.InClass _ (T.TypeVar _)) -> (p':) <$> reducePreds loc ps
    (T.InClass tc (T.TypeCon sym _))
      | inClass tc sym -> reducePreds loc ps
    (T.NatJoin _ b c)
      | isRowVar b || isRowVar c -> (p':) <$> reducePreds loc ps
    (T.NatJoin a (T.TypeRow f1 Nothing) (T.TypeRow f2 Nothing)) -> do
      reduceJoin p' a f1 f2
      reducePreds loc ps
    _ -> throwError (InferPredViolation (At loc [p']))
  where
    numTypes = ["~int2", "~int4", "~int8", "~numeric", "~float4", "~float8"]
    isDbValue = T.isPrefixOf "~"
    isComparableType = isDbValue

    inClass T.Num sym = sym `elem` numTypes
    inClass T.Comparable sym = isComparableType sym

    isRowVar = \case
      (T.TypeVar _) -> True
      (T.TypeRow _ (Just _)) -> True
      _ -> False

    reduceJoin :: T.Pred -> T.Type -> M.Map AST.Symbol T.Type -> M.Map AST.Symbol T.Type -> TypeCheck InferError ()
    reduceJoin p' a f1 f2
      | S.null (S.intersection (M.keysSet f1) (M.keysSet f2)) =
        throwError (InferPredViolation (At loc [p']))
      | otherwise = do
          forM_ (M.elems $ M.intersectionWith (,) f1 f2) (uncurry (unify loc))
          unify loc a (T.TypeRow (M.union f1 f2) Nothing)
  

solve :: [T.Pred] -> [Located Constraints.Constraint] -> TypeCheck InferError [T.Pred]
solve preds [] = return preds
solve preds (c:cs) =
  case c of
    (At consSpan (Constraints.ConsEq t1 t2)) -> do
      unify consSpan t1 t2
      preds' <- reducePreds consSpan preds
      solve preds' cs


inferExpression :: Env.TypeEnv -> AST.Expression -> Either InferError T.Qual
inferExpression env ast = runTypeCheck $ do
  (assumps, constraints, t) <- Constraints.expressionConstraints ast
  preds <- instantiateAll env assumps
  preds' <- solve preds constraints
  t' <- updateType t
  return $ T.Qual (nub preds') t'


inferDefinition :: Env.TypeEnv -> AST.Definition -> Either InferError T.TypeSchema
inferDefinition env def = runTypeCheck $ do
  (assumps, constraints, t) <- Constraints.definitionConstraints def
  preds <- instantiateAll env assumps
  preds' <- solve preds constraints
  t' <- updateType t
  return $ T.generalise S.empty $ T.Qual (nub preds') t'
