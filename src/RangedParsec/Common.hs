module RangedParsec.Common where

import Data.Char (isDigit)
import qualified Data.Set as S
import qualified Data.Text as T

import RangedParsec.Pos
import RangedParsec.Parsec


eof :: Parsec ()
eof =
  expecting "end of input" $
  Parsec $ \s@(ParseState { psInput }) ->
  if T.null psInput
  then Empty (Ok () S.empty s)
  else Empty (Error S.empty s)


char :: Char -> Parsec ()
char ch =
  expecting (T.pack ['\'', ch, '\'']) $
  Parsec $ \s@(ParseState { psInput, psPos }) ->
  case T.uncons psInput of
    Nothing -> Empty (Error S.empty s)
    Just (ch', remaining)
      | ch == ch' ->
        let pos' = nextCol psPos
        in Consumed (Ok () S.empty (consume s remaining pos'))
    _ -> Empty (Error S.empty s)


integer :: Parsec Integer
integer =
  expecting "integer" $
  Parsec $ \s@(ParseState { psInput, psPos }) ->
  case T.uncons psInput of
    Just ('-', input') ->
      case T.span isDigit input' of
        (i, remaining)
          | T.null i ->
            Empty (Error S.empty s)
          | otherwise ->
            let pos' = addCol (T.length i + 1) psPos
            in Consumed (Ok (-(read (T.unpack i))) S.empty (consume s remaining pos'))
    _ -> 
      case T.span isDigit psInput of
        (i, remaining)
          | T.null i ->
            Empty (Error S.empty s)
          | otherwise ->
            let pos' = addCol (T.length i) psPos
            in Consumed (Ok (read (T.unpack i)) S.empty (consume s remaining pos'))


quotedString :: Parsec T.Text
quotedString =
  expecting "string" $
  Parsec $ \s@(ParseState { psInput, psPos }) ->
  case T.uncons psInput of
    Just ('\'', remaining) ->
      let pos' = nextCol psPos
      in Consumed $ loop T.empty (consume s remaining pos')
    _ -> Empty (Error S.empty s)
  where
    loop :: T.Text -> ParseState -> Reply T.Text
    loop acc s@(ParseState { psInput, psPos }) =
      case T.uncons psInput of
        Nothing -> Error S.empty s
        Just ('\\', input) ->
          case T.uncons input of
            Nothing -> Error S.empty s
            Just (ch, input') ->
              let p' = nextCol (nextCol psPos)
              in loop (T.cons ch acc) (consume s input' p')
        Just ('\'', input) ->
          let p' = nextCol psPos
          in Ok (T.reverse acc) S.empty (consume s input p')
        Just (ch, input) ->
          let p' = if ch == '\n' then nextLine psPos else nextCol psPos
          in loop (T.cons ch acc) (consume s input p')
