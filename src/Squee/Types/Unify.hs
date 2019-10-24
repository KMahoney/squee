module Squee.Types.Unify where

import Prelude hiding (span)
import qualified Data.Map as M
import Control.Monad.Except

import Squee.AST (Symbol)
import Squee.Types.Type
import Squee.Types.Monad.TypeCheck


data UnificationError
  = UnificationError Type Type
  | MissingFields [Symbol]
  deriving (Eq, Show)

type Unifier a = TypeCheck UnificationError a


unify :: Type -> Type -> Unifier ()
unify t1 t2 = do
  t1' <- updateType t1
  t2' <- updateType t2
  case (t1', t2') of
    (TypeVar i, t) -> updateSubst (M.singleton i t)
    (t, TypeVar i) -> updateSubst (M.singleton i t)
    (TypeRow f1 v1, TypeRow f2 v2) -> do
      forM_ (M.elems $ M.intersectionWith (,) f1 f2) (uncurry unify)
      v <- freshVarInt
      rowDifference (M.difference f1 f2) v2 (fmap (const v) v1)
      rowDifference (M.difference f2 f1) v1 (fmap (const v) v2)
    (TypeCon cons1 args1, TypeCon cons2 args2)
      | cons1 == cons2 && length args1 == length args2 ->
          mapM_ (uncurry unify) (zip args1 args2)
    _ -> throwError (UnificationError t1' t2')


rowDifference :: M.Map Symbol Type -> Maybe Int -> Maybe Int -> Unifier ()
rowDifference fields Nothing _
  | M.null fields = return ()
  | otherwise = throwError (MissingFields (M.keys fields))
rowDifference fields (Just i) newVar =
  updateSubst (M.singleton i (TypeRow fields newVar))
