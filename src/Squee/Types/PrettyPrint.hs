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
showPred = \case
  InClass tc t -> "(" <> className tc <> " " <> showType t <> ")"
  ValuesInClass tc t -> "(" <> className tc <> " v, (k, v) ∈ " <> showType t <> ")"
  NatJoin a b c -> "({" <> showType a <> "} = {" <> showType b <> "} ⋈ {" <> showType c <> "})"
  AggValues a b -> "({" <> showType a <> "} = Agg {" <> showType b <> "})"

  where
    className = \case
      Num -> "Num"
      Comparable -> "Comparable"
      DbValue -> "DB"


showQual :: Qual -> Text
showQual (Qual preds t) =
  case filter (not . isDbRow) preds of
    [] -> showType t
    preds' -> intercalate " " (map showPred preds') <> " ⇒ " <> showType t
  where
    -- Don't show the DB-row restriction since it's so common and causes a lot of noise
    -- TODO: find a better way of showing this
    isDbRow = \case
      ValuesInClass DbValue _ -> True
      _ -> False


showSchema :: TypeSchema -> Text
showSchema (TypeSchema _ q) = showQual q
