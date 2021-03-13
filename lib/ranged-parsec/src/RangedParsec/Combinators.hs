module RangedParsec.Combinators
  ( skipSpace
  , takeWhile
  , takeWhile1
  , excluding
  , excludingSet
  , sepBy
  , sepBy1
  , manyTill
  , try
  , matchSpan
  , chain
  ) where

import Prelude hiding (takeWhile)
import Data.Char (isSpace)
import qualified Data.Set as S
import Control.Applicative
import Data.Text (Text)
import qualified Data.Text as T

import RangedParsec.Pos
import RangedParsec.Parsec


psSkipSpace :: ParseState -> ParseState
psSkipSpace (s@ParseState { psInput, psPos }) = loop psInput psPos
  where
    loop input p =
      case T.uncons input of
         Just ('\n', remaining) -> loop remaining (nextLine p)
         Just (ch, remaining) | isSpace ch -> loop remaining (nextCol p)
         _ -> s { psInput = input, psPos = p }


skipSpace :: Parsec ()
skipSpace = Parsec $ \s -> Empty (Ok () S.empty (psSkipSpace s))


takeWhile :: (Char -> Bool) -> Parsec Text
takeWhile p = Parsec (loop T.empty)
  where
    loop :: Text -> ParseState -> Consumed (Reply Text)
    loop acc (s@ParseState { psInput, psPos }) =
      case T.uncons psInput of
        Just (ch, remaining)
          | p ch ->
              let pos' = if ch == '\n' then nextLine psPos else nextCol psPos
              in loop (T.cons ch acc) (consume s remaining pos')

        _ ->
          Consumed (Ok (T.reverse acc) S.empty s)


takeWhile1 :: (Char -> Bool) -> Parsec Text
takeWhile1 p = Parsec (loop T.empty)
  where
    loop :: Text -> ParseState -> Consumed (Reply Text)
    loop acc (s@ParseState { psInput, psPos }) =
      case T.uncons psInput of
        Just (ch, remaining)
          | p ch ->
              let pos' = if ch == '\n' then nextLine psPos else nextCol psPos
              in loop (T.cons ch acc) (consume s remaining pos')

        _
          | T.null acc ->
            Empty (Error S.empty s)
          | otherwise ->
            Consumed (Ok (T.reverse acc) S.empty s)


excluding :: Parsec a -> Parsec ()
excluding p =
  Parsec $ \s ->
  case runParsec p s of
    Consumed (Ok _ _ _) -> Empty (Error S.empty s)
    Consumed (Error _ _) -> Empty (Ok () S.empty s)
    Empty (Ok _ _ _) -> Empty (Error S.empty s)
    Empty (Error _ _) -> Empty (Ok () S.empty s)


excludingSet :: Parsec Text -> S.Set Text -> Parsec Text
excludingSet p set =
  Parsec $ \s ->
  case runParsec p s of
    Consumed (Ok x _ _)
      | S.member x set -> Empty (Error S.empty s)
    r -> r


sepBy :: Parsec a -> Parsec b -> Parsec [a]
sepBy p sep = loop1 []
  where
    loop1 acc =
      (p >>= \next -> loop2 (next:acc)) <|> pure (reverse acc)
    loop2 acc =
      (sep *> loop1 acc) <|> pure (reverse acc)


sepBy1 :: Parsec a -> Parsec b -> Parsec [a]
sepBy1 p sep = loop []
  where
    loop acc = do
      next <- p
      (sep *> loop (next:acc)) <|> pure (reverse (next:acc))


chain :: (b -> a -> a -> a) -> Parsec b -> Parsec a -> Parsec a
chain f sep p = p >>= loop1
  where
    loop1 acc = loop2 acc <|> pure acc
    loop2 acc = (f <$> sep <*> pure acc <*> p) >>= loop1


manyTill :: Parsec a -> Parsec b -> Parsec [a]
manyTill p end =
  (end >> return []) <|> ((:) <$> p <*> manyTill p end)


try :: Parsec a -> Parsec a
try (Parsec p) =
  Parsec $ \s ->
  case p s of
    Consumed (Error ex _) -> Empty (Error ex s)
    Consumed x -> Consumed x
    x -> x


matchSpan :: (Char -> Bool) -> Text -> Parsec Text
matchSpan f x = Parsec $ \s@(ParseState { psInput, psPos }) ->
  case T.span f psInput of
    (x', remaining)
      | x == x' ->
        let pos' = addCol (T.length x) psPos
        in Consumed (Ok x S.empty (consume s remaining pos'))
    _ -> Empty (Error S.empty s)
