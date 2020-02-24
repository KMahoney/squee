module Squee.Types.PrettyPrint (showType, showPred, showQual, showSchema) where

import Data.Text (Text, intercalate)
import qualified Data.Text as T
import qualified Data.Map as M

import qualified Squee.AST as AST
import Squee.Types.Type


varNames :: [Text]
varNames = map T.singleton ['α'..'Ͽ']


showType :: Type -> Text
showType (TypeVar i) = varNames !! i
showType (TypeCon "->" [a@(TypeCon "->" _), b]) = "(" <> showType a <> ") → " <> showType b
showType (TypeCon "->" [a, b]) = showType a <> " → " <> showType b
showType (TypeCon "Row" [r]) = "{" <> showType r <> "}"
showType (TypeCon "Query" [r]) = "[" <> showType r <> "]"
showType (TypeCon tName args) = tName <> mconcat (map ((" " <>) . showType) args)
showType (TypeRow fields var) =
  intercalate ", " (map (\(sym, t) -> AST.symbolName sym <> ": " <> showType t) (M.toList fields)) <>
  maybe T.empty ((", .." <>) . (varNames !!)) var


showPred :: Pred -> Text
showPred (Num t) = "(Num " <> showType t <> ")"
showPred (Comparable t) = "(Comparable " <> showType t <> ")"
showPred (NatJoin a b c) = "({" <> showType a <> "} = {" <> showType b <> "} ⋈ {" <> showType c <> "})"


showQual :: Qual -> Text
showQual (Qual [] t) = showType t
showQual (Qual preds t) = intercalate " " (map showPred preds) <> " ⇒ " <> showType t


showSchema :: TypeSchema -> Text
showSchema (TypeSchema _ q) = showQual q
