module Database.Schema.Structure
  ( Schema(..)
  , TableName(..)
  , ColumnName(..)
  , Table
  , Column(..)
  , ColumnReference(..)
  , empty
  , lookupTable
  , tableNames
  ) where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Encoding as JSON
import Data.Text (Text)
import qualified Data.Char as Char
import GHC.Generics

import Database.Schema.DBType

newtype TableName = TableName { tableName :: Text }
  deriving (Ord, Show, Eq)

newtype ColumnName = ColumnName { columnName :: Text }
  deriving (Ord, Show, Eq)

data ColumnReference =
  ColumnReference { refTable :: TableName, refColumn :: ColumnName }
  deriving (Ord, Show, Eq)

data Schema = Schema
  { schemaTables :: M.Map TableName Table
  , schemaSequences :: S.Set Text
  } deriving (Show, Eq, Generic)

type Table = M.Map ColumnName Column

data Column = Column
  { columnType :: DBType
  } deriving (Show, Eq, Generic)


instance JSON.FromJSON ColumnName where
  parseJSON = JSON.withText "column name" (return . ColumnName)

instance JSON.FromJSONKey ColumnName where
  fromJSONKey = JSON.FromJSONKeyText ColumnName

instance JSON.ToJSON ColumnName where
  toJSON = JSON.String . columnName

instance JSON.ToJSONKey ColumnName where
  toJSONKey = JSON.ToJSONKeyText columnName (JSON.text . columnName)

instance JSON.FromJSON TableName where
  parseJSON = JSON.withText "column name" (return . TableName)

instance JSON.FromJSONKey TableName where
  fromJSONKey = JSON.FromJSONKeyText TableName

instance JSON.ToJSON TableName where
  toJSON = JSON.String . tableName

instance JSON.ToJSONKey TableName where
  toJSONKey = JSON.ToJSONKeyText tableName (JSON.text . tableName)

columnOpts :: JSON.Options
columnOpts = JSON.defaultOptions { JSON.fieldLabelModifier = map Char.toLower . drop 6 }

instance JSON.FromJSON Column where
  parseJSON = JSON.genericParseJSON columnOpts

instance JSON.ToJSON Column where
  toJSON = JSON.genericToJSON columnOpts
  toEncoding = JSON.genericToEncoding columnOpts

schemaOpts :: JSON.Options
schemaOpts = JSON.defaultOptions { JSON.fieldLabelModifier = map Char.toLower . drop 6 }

instance JSON.FromJSON Schema where
  parseJSON = JSON.genericParseJSON schemaOpts

instance JSON.ToJSON Schema where
  toJSON = JSON.genericToJSON schemaOpts
  toEncoding = JSON.genericToEncoding schemaOpts

empty :: Schema
empty = Schema M.empty S.empty


lookupTable :: TableName -> Schema -> Maybe Table
lookupTable tableName schema = M.lookup tableName (schemaTables schema)


tableNames :: Schema -> [TableName]
tableNames = M.keys . schemaTables
