module Cli.Pretty.InferError where

import qualified Data.Text as T
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal

import qualified Squee.Types.Unify as Type
import qualified Squee.Types.Infer as Type
import qualified Squee.Types.PrettyPrint as Type

import qualified Squee.AST as AST
import qualified RangedParsec as RP
import RangedParsec (Located(..))


showInferError :: Type.InferError -> Doc AnsiStyle
showInferError = \case
      Type.InferUnknown (At errSpan _) -> do
        "Unknown variable" <> line <>
          RP.prettyRange errSpan
      Type.InferUnificationError (At errSpan (Type.UnificationError a b)) -> do
        "Cannot unify " <> pretty (Type.showType a) <> " with " <> pretty (Type.showType b) <> line <>
          RP.prettyRange errSpan
      Type.InferUnificationError (At errSpan (Type.MissingFields fields)) -> do
        "Missing fields " <> pretty (T.intercalate ", " (map AST.symbolName fields)) <> line <>
          RP.prettyRange errSpan
      Type.InferPredViolation (At errSpan preds) -> do
        "Violates constraint(s) " <> pretty (T.intercalate " " (map Type.showPred preds)) <> line <>
          RP.prettyRange errSpan
