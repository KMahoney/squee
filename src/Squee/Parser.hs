module Squee.Parser
  ( isIdentifierChar -- used in repl to start completion
  , parseReplStatement
  , parseDefinitions
  ) where

import Control.Applicative
import Data.Char (isAlphaNum)
import qualified Data.Text as T
import qualified Data.Set as S

import RangedParsec.Parsec
import RangedParsec

import Squee.AST


parseReplStatement :: T.Text -> Either ParseError ReplStatement
parseReplStatement = parse replStatement "REPL"


parseDefinitions :: T.Text -> T.Text -> Either ParseError Definitions
parseDefinitions = parse definitions


replStatement :: Parsec ReplStatement
replStatement = skipSpace *> (replDefinition <|> replExpression) <* eof
  where
    replDefinition = RSDefinition <$> definition
    replExpression = RSExpression <$> expression


definitions :: Parsec Definitions
definitions = skipSpace *> (manyTill definition eof)


definition :: Parsec Definition
definition = LocalDef <$> (keyword "def" *> identifier) <*> manyTill identifier (operator ":=") <*> expression


isIdentifierChar :: Char -> Bool
isIdentifierChar c = isAlphaNum c || c `elem` ("_" :: String)


isOperatorChar :: Char -> Bool
isOperatorChar c = c `elem` (":+-<>|=/*" :: String)


operator :: T.Text -> Parsec Symbol
operator op = expecting ("operator " <> op) $
  Symbol <$> matchSpan isOperatorChar op <* skipSpace


identifier :: Parsec Symbol
identifier = expecting "identifier" $
  Symbol <$> (takeWhile1 isIdentifierChar `excludingSet` keywords) <* skipSpace
  where
    keywords = S.fromList [ "def" ]


keyword :: T.Text -> Parsec ()
keyword kw = expecting kw $ matchSpan isIdentifierChar kw >> skipSpace


lexChar :: Char -> Parsec ()
lexChar ch =
  char ch >> skipSpace


expression :: Parsec Expression
expression = terms


terms :: Parsec Expression
terms = pipe $ eq $ comp $ addSubtract $ mulDivide $ applications
  where
    mulDivide = chain BinOp (parseLocated (operator "*" <|> operator "/"))
    addSubtract = chain BinOp (parseLocated (operator "+" <|> operator "-"))
    comp = chain BinOp (parseLocated (operator "<" <|> operator ">"))
    eq = chain BinOp (parseLocated (operator "="))
    pipe = chain BinOp (parseLocated (operator "|"))


applications :: Parsec Expression
applications = simpleExpression >>= args
  where
    args :: Expression -> Parsec Expression
    args e = ((App e <$> (parseLocated simpleExpression)) >>= args) <|> pure e


simpleExpression :: Parsec Expression
simpleExpression = (litInt <|> litString <|> litRow <|> var <|> lambda <|> parens) >>= fields
  where
    var = Var <$> parseLocated identifier
    litString = (Lit . LitString) <$> quotedString <* skipSpace
    litRow = (Lit . LitRow) <$> (lexChar '{' *> sepBy rowField (lexChar ',') <* lexChar '}')
    lambda = lexChar '\\' *> (Abs <$> some identifier <*> (operator "->" *> expression))
    parens = lexChar '(' *> expression <* lexChar ')'
    litInt = (Lit . LitInt) <$> integer <* skipSpace

    rowField ::Parsec  (Symbol, Expression)
    rowField = (,) <$> identifier <* lexChar ':' <*> expression

    fields :: Expression -> Parsec Expression
    fields e = ((Field e <$> (lexChar '.' *> parseLocated identifier)) >>= fields) <|> pure e
