{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module RangedParsec.Pos
  ( Line
  , Column
  , Pos
  , Range
  , Sourced(..)
  , SourceSpan
  , SourcePos
  , Located(..)
  , initialPos
  , nextLine
  , nextCol
  , addCol
  , firstSourcePos
  ) where

import Data.Text (Text)


type Line = Int
type Column = Int
type Pos = (Line, Column)
type Range = (Pos, Pos)


initialPos :: Pos
initialPos = (1, 1)


nextLine :: Pos -> Pos
nextLine (line, _) = (line + 1, 1)


nextCol :: Pos -> Pos
nextCol = addCol 1


addCol :: Int -> Pos -> Pos
addCol i (line, col) = (line, col + i)


data Sourced a =
  Sourced { sourceFilename :: Text
          , sourceContent :: Text
          , discardSource :: a
          }
  deriving (Functor, Foldable, Traversable)

type SourceSpan = Sourced Range
type SourcePos = Sourced Pos


data Located a = At { locatedSpan :: SourceSpan, discardLocation :: a }
  deriving (Functor, Foldable, Traversable)

instance Eq a => Eq (Located a) where
  (At _ a) == (At _ b) = a == b

instance Show a => Show (Located a) where
  show = show . discardLocation


firstSourcePos :: SourceSpan -> SourcePos
firstSourcePos (Sourced { sourceFilename, sourceContent, discardSource = (pos, _) }) =
  Sourced { sourceFilename, sourceContent, discardSource = pos  }
