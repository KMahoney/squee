module Squee.Types.CommonType
  ( tText
  , tInt4
  , tFn
  , tBool
  , tQuery
  , tRow
  , tRowList
  ) where

import qualified Data.Map as M
import qualified Squee.AST as AST
import qualified Squee.Types.Type as T


tText :: T.Type
tText = T.TypeCon "~text" []

tBool :: T.Type
tBool = T.TypeCon "~bool" []

tFn :: T.Type -> T.Type -> T.Type
tFn a b = T.TypeCon "->" [a, b]

tInt4 :: T.Type
tInt4 = T.TypeCon "~int4" []

tRow :: T.Type -> T.Type
tRow t = T.TypeCon "Row" [t]

tQuery :: T.Type -> T.Type
tQuery t = T.TypeCon "Query" [t]

tRowList :: [(AST.Symbol, T.Type)] -> T.Type
tRowList = tRow . flip T.TypeRow Nothing . M.fromList
