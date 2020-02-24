module Squee.Types.Type where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Squee.AST as AST

import Data.Text (Text)


data Type
  = TypeCon Text [Type]
  | TypeVar Int
  | TypeRow (M.Map AST.Symbol Type) (Maybe Int)
  deriving (Show, Eq)

data Qual = Qual [Pred] Type
  deriving (Show, Eq)

data Pred
  = Num Type
  | Comparable Type
  | NatJoin Type Type Type
  deriving (Show, Eq)

data TypeSchema = TypeSchema [Int] Qual
  deriving (Show, Eq)

type Subst = M.Map Int Type


class Types a where
  tv :: a -> S.Set Int
  applySubst :: Subst -> a -> a
  
instance Types Type where
  tv t = case t of
    TypeCon _ args -> S.unions (map tv args)
    TypeVar i -> S.singleton i
    TypeRow r v -> S.union (tv r) (maybe S.empty S.singleton v)
  applySubst s t = case t of
    TypeVar i ->
      case M.lookup i s of
        Just t' -> t'
        Nothing -> t
    TypeCon tName args ->
      TypeCon tName (map (applySubst s) args)
    TypeRow row v ->
      let row' = applySubst s row in
        case v >>= flip M.lookup s of
          Just (TypeCon _ _) ->
            error "cannot substitute row variable for type constructor"
          Just (TypeVar i) ->
            TypeRow row' (Just i)
          Just (TypeRow row'' v')
            | S.null (S.intersection (M.keysSet row') (M.keysSet row'')) ->
              TypeRow (M.union row' row'') v'
            | otherwise ->
              error "cannot substitute row with overlapping keys"
          Nothing ->
            TypeRow row' v

instance Types Pred where
  tv (Num t) = tv t
  tv (Comparable t) = tv t
  tv (NatJoin a b c) = S.unions (map tv [a, b, c])
  applySubst s (Num t) = Num (applySubst s t)
  applySubst s (Comparable t) = Comparable (applySubst s t)
  applySubst s (NatJoin a b c) = NatJoin (applySubst s a) (applySubst s b) (applySubst s c)

instance Types Qual where
  tv (Qual preds t) = S.union (tv preds) (tv t)
  applySubst s (Qual preds t) = Qual (applySubst s preds) (applySubst s t)

instance Types TypeSchema where
  tv (TypeSchema boundVars t) = S.difference (tv t) (S.fromList boundVars)
  applySubst s (TypeSchema boundVars t) =
    TypeSchema boundVars (applySubst (s `M.withoutKeys` S.fromList boundVars) t)

instance Types v => Types [v] where
  tv ts = S.unions (map tv ts)
  applySubst s m = fmap (applySubst s) m

instance Types v => Types (M.Map k v) where
  tv m = S.unions (map tv (M.elems m))
  applySubst s m = fmap (applySubst s) m


composeSubst :: Subst -> Subst -> Subst
composeSubst s1 s2 = (M.map (applySubst s1) s2) `M.union` s1


normaliseTyVars :: Types t => t -> t
normaliseTyVars t = applySubst s t
  where s = M.fromList (zip (S.toList (tv t)) (map TypeVar [0..]))


generalise :: S.Set Int -> Qual -> TypeSchema
generalise freeVars t = TypeSchema (S.toList $ S.difference (tv t) freeVars) t
