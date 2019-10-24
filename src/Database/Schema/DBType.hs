module Database.Schema.DBType (DBType(..)) where

import GHC.Generics
import Data.Text (Text)
import qualified Data.Aeson as JSON

data DBType
  = DBType { nullable :: Bool
           , typename :: Text
           }
  deriving (Eq, Ord, Show, Generic)

instance JSON.FromJSON DBType
instance JSON.ToJSON DBType
