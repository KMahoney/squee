module Database.Sql (Sql(..), (<+>), quoteName, quoteString, intercalate, hcat, toPGQuery) where

import qualified Database.PostgreSQL.Simple as PG

import Data.String
import Data.List (intersperse)
import Data.Text (Text)
import qualified Data.Text as T


data Sql = Sql { toText :: Text }
  deriving (Show)


instance Semigroup Sql where
  (Sql a) <> (Sql b) = Sql (a <> b)

instance IsString Sql where
  fromString = Sql . T.pack

instance Monoid Sql where
  mempty = Sql ""


(<+>) :: Sql -> Sql -> Sql
(Sql a) <+> (Sql b) = Sql (a <> " " <> b)


quoteName :: Text -> Sql
quoteName s = Sql $ "\"" <> T.concatMap escape s <> "\""
  where
    escape :: Char -> Text
    escape '"' = "\\\""
    escape x = T.singleton x


quoteString :: Text -> Sql
quoteString s = Sql $ "'" <> T.concatMap escape s <> "'"
  where
    escape :: Char -> Text
    escape '\'' = "\\'"
    escape x = T.singleton x


intercalate :: Sql -> [Sql] -> Sql
intercalate sep sqls = mconcat (intersperse sep sqls)


hcat :: [Sql] -> Sql
hcat = mconcat


toPGQuery :: Sql -> PG.Query
toPGQuery = fromString . T.unpack . toText
