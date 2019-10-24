module RangedParsec.Parsec
  ( Parsec(..)
  , ParseState(..)
  , Consumed(..)
  , Reply(..)
  , parse
  , consume
  , getPos
  , getSpan
  , parseLocated
  , parseError
  , expecting
  , hidden
  ) where

import qualified Data.Set as S
import Data.Text (Text)
import Control.Monad
import Control.Applicative

import RangedParsec.Pos
import RangedParsec.Error


data Parsec a
  = Parsec { runParsec :: ParseState -> Consumed (Reply a) }

data Consumed a
  = Consumed a
  | Empty !a
  deriving (Show)

data Reply a
  = Ok !a (S.Set Text) !ParseState
  | Error (S.Set Text) !ParseState
  deriving (Show)

data ParseState = ParseState { psInput :: Text
                             , psPos :: Pos
                             , psLastPos :: Pos
                             , psFilename :: Text
                             , psContent :: Text
                             }
  deriving (Eq, Show)


instance Functor Consumed where
  fmap f (Consumed x) = Consumed (f x)
  fmap f (Empty x) = Empty (f x)

instance Functor Reply where
  fmap f (Ok x e s) = Ok (f x) e s
  fmap _ (Error e s) = Error e s

instance Functor Parsec where
  fmap = pMap

instance Monad Parsec where
  (>>=) = pBind

instance Applicative Parsec where
  pure = pReturn
  (<*>) = ap

instance Alternative Parsec where
  empty = parseError S.empty
  (<|>) = pPlus


pMap :: (a -> b) -> Parsec a -> Parsec b
pMap f p = Parsec $ \s -> fmap (fmap f) (runParsec p s)


pReturn :: a -> Parsec a
pReturn x = Parsec $ \s -> Empty $ Ok x S.empty s


pBind :: Parsec a -> (a -> Parsec b) -> Parsec b
pBind p f =
  Parsec $ \s ->
    case runParsec p s of
      Empty (Ok x e1 s') ->
        case runParsec (f x) s' of
          Empty (Ok x2 e2 s'') -> Empty (Ok x2 (S.union e1 e2) s'')
          Empty (Error e2 s'') -> Empty (Error (S.union e1 e2) s'')
          other -> other
      Empty (Error e s') -> Empty (Error e s')
      Consumed (Ok x e1 s') ->
        Consumed $ case runParsec (f x) s' of
                     Empty (Ok x2 e2 s'') -> Ok x2 (S.union e1 e2) s''
                     Empty (Error e2 s'') -> Error (S.union e1 e2) s''
                     Consumed reply -> reply
      Consumed (Error e s') -> Consumed (Error e s')


pPlus :: Parsec a -> Parsec a -> Parsec a
pPlus (Parsec p1) (Parsec p2) =
  Parsec $ \s ->
  case p1 s of
    Empty (Error e1 _) ->
      case p2 s of
        Empty (Ok a e2 s2) -> Empty (Ok a (S.union e1 e2) s2)
        Empty (Error e2 _) -> Empty (Error (S.union e1 e2) s)
        other -> other
    other -> other


parse :: Parsec a -> Text -> Text -> Either ParseError a
parse p filename input =
  case runParsec p (ParseState input initialPos initialPos filename input) of
    Consumed (Ok result _ _) -> Right result
    Consumed (Error e s) -> Left (err e s)
    Empty (Ok result _ _) -> Right result
    Empty (Error e s) -> Left (err e s)

  where
    err e s = ParseError (Sourced (psFilename s) (psContent s) (psPos s)) e


consume :: ParseState -> Text -> Pos -> ParseState
consume s remaining p = s { psInput = remaining, psPos = p, psLastPos = p }


getPos :: Parsec Pos
getPos = Parsec $ \s -> Empty (Ok (psPos s) S.empty s)


getSpan :: Pos -> Parsec SourceSpan
getSpan start =
  Parsec $ \s ->
  let sp = Sourced (psFilename s) (psContent s) (start, psLastPos s)
  in Empty (Ok sp S.empty s)


parseLocated :: Parsec a -> Parsec (Located a)
parseLocated p = do
  start <- getPos
  result <- p
  resultSpan <- getSpan start
  return (At resultSpan result)


parseError :: S.Set Text -> Parsec a
parseError e = Parsec $ \s -> Empty $ Error e s


expecting :: Text -> Parsec a -> Parsec a
expecting e p =
  Parsec $ \s ->
  case runParsec p s of
    Empty (Ok x _ s') -> Empty (Ok x (S.singleton e) s')
    Empty (Error _ s') -> Empty (Error (S.singleton e) s')
    Consumed (Ok x e' s') -> Consumed (Ok x e' s')
    Consumed (Error e' s') -> Consumed (Error e' s')


hidden :: Parsec a -> Parsec a
hidden p =
  Parsec $ \s ->
  case runParsec p s of
    Empty (Ok x _ s') -> Empty (Ok x S.empty s')
    Empty (Error _ s') -> Empty (Error S.empty s')
    Consumed (Ok x e' s') -> Consumed (Ok x e' s')
    Consumed (Error e' s') -> Consumed (Error e' s')
