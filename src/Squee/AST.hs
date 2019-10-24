module Squee.AST
  ( Symbol(..)
  , LitValue(..)
  , Expression(..)
  , ReplStatement(..)
  , Definitions
  ) where

import Data.Text (Text)
import RangedParsec (Located)

data Symbol = Symbol { symbolName :: Text }
    deriving (Show, Eq, Ord)

data LitValue
  = LitString Text
  | LitInt Integer
  | LitRow [(Symbol, Expression)]
    deriving (Show)

data Expression
  = Var (Located Symbol)
  | App Expression (Located Expression)
  | Abs [Symbol] Expression
  | Field Expression (Located Symbol)
  | BinOp (Located Symbol) Expression Expression
  | Lit LitValue
    deriving (Show)

data ReplStatement
  = RSAssignment Symbol Expression
  | RSExpression Expression
  
type Definitions
  = [(Symbol, Expression)]
