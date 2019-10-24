module RangedParsec.Error where

import Prelude hiding (span)

import Data.Text (Text, unpack)
import qualified Data.Set as S
import RangedParsec.Pos


data ParseError =
  ParseError { errSourcePos :: SourcePos
             , errExpecting :: S.Set Text
             }


instance Show ParseError where
  show err = unpack $ errMessage err


errMessage :: ParseError -> Text
errMessage (ParseError { errExpecting }) =
  "Expecting " <> exList

  where
    exList = orList (S.toList errExpecting)

    orList [] = ""
    orList [a] = a
    orList [a, b] = a <> " or " <> b
    orList (h:t) = h <> ", " <> orList t
