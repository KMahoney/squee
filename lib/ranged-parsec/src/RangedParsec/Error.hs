module RangedParsec.Error where

import Prelude hiding (span)

import Data.Text (Text, unpack, pack)
import qualified Data.Set as S
import RangedParsec.Pos


data ParseError =
  ParseError { errSourcePos :: SourcePos
             , errExpecting :: S.Set Text
             }


instance Show ParseError where
  show err = unpack $ errMessage err


errMessage :: ParseError -> Text
errMessage (ParseError { errExpecting, errSourcePos }) =
  location <> "Expecting " <> exList

  where
    exList = orList (S.toList errExpecting)

    orList [] = ""
    orList [a] = a
    orList [a, b] = a <> " or " <> b
    orList (h:t) = h <> ", " <> orList t

    (line, col) =
      discardSource errSourcePos

    location =
      sourceFilename errSourcePos <> ":" <>
      pack (show line) <> ":" <> pack (show col) <> ": "
