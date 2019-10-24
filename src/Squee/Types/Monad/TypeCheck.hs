module Squee.Types.Monad.TypeCheck
  ( TypeCheck
  , mapError
  , runTypeCheck
  , freshVarInt
  , freshVar
  , getSubst
  , putSubst
  , modifySubst
  , updateSubst
  , updateType
  , instantiate
  ) where

import qualified Data.Map as M
import Control.Monad.Except
import Control.Monad.State

import qualified Squee.Types.Type as T

data TypeCheckState = TypeCheckState { nextFreshVar :: Int, currentSubst :: T.Subst }
type TypeCheck err a = StateT TypeCheckState (Except err) a


initialState :: TypeCheckState
initialState = TypeCheckState { nextFreshVar = 0, currentSubst = M.empty }


mapError :: (a -> b) -> TypeCheck a x -> TypeCheck b x
mapError f = mapStateT (withExcept f)


runTypeCheck :: TypeCheck err a -> Either err a
runTypeCheck = runExcept . flip evalStateT initialState


freshVarInt :: TypeCheck err Int
freshVarInt = do
  i <- gets nextFreshVar
  modify $ \s -> s { nextFreshVar = nextFreshVar s + 1 }
  return i


freshVar :: TypeCheck err T.Type
freshVar = T.TypeVar <$> freshVarInt


getSubst :: TypeCheck err T.Subst
getSubst = gets currentSubst


putSubst :: T.Subst -> TypeCheck err ()
putSubst subst = modify $ \s -> s { currentSubst = subst }


modifySubst :: (T.Subst -> T.Subst) -> TypeCheck err ()
modifySubst f = modify $ \s -> s { currentSubst = f (currentSubst s) }


updateSubst :: T.Subst -> TypeCheck err ()
updateSubst = modifySubst . T.composeSubst


updateType :: T.Types t => t -> TypeCheck err t
updateType t = do
  s <- getSubst
  return (T.applySubst s t)


instantiate :: T.TypeSchema -> TypeCheck err T.Qual
instantiate (T.TypeSchema forall t) = do
  freshVars <- replicateM (length forall) freshVar
  let s = M.fromList (zip forall freshVars)
  return (T.applySubst s t)
