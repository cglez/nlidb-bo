{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Data.SemQuery
  ( SemQuery(..)
  , Target(..)
  , Function(..)
  , Condition(..)
  , Operator(..)
  , LogicOperator(..)
  ) where

import           GHC.Generics                (Generic)
import qualified Data.Text                   as TS (Text)
import           Data.Morpheus.Kind          (INPUT_OBJECT, ENUM)
import           Data.Morpheus.Types         (GQLType(..))


data SemQuery =
  SemQuery
      { command    :: TS.Text
      , targets    :: [Target]
      , conditions :: [Condition]
      }
  deriving (Generic)

instance GQLType SemQuery where
  type KIND SemQuery = INPUT_OBJECT
  description _ = "A semantic representation of an information query"


-- TODO: use Command instead of Text - problem: variables object doesn't allow literals
data Command
  = SELECT
  | INSERT
  | UPDATE
  | DELETE
  deriving (Generic, Show)

instance GQLType Command where
  type KIND Command = ENUM


data Target =
    Target { argument :: TS.Text
           , function :: Maybe TS.Text
           }
    deriving (Generic)

instance GQLType Target where
  type KIND Target = INPUT_OBJECT


data Condition =
  Condition { subject :: TS.Text
            , operator :: TS.Text
            , value :: TS.Text
            }
  deriving (Generic)

instance GQLType Condition where
  type KIND Condition = INPUT_OBJECT


data Operator
  = EQ
  | NEQ
  | GT
  | GTE
  | LT
  | LTE
  | IN
  | BETWEEN
  | LIKE
  | IS_NULL
  | NOT_NULL
  deriving (Generic, Show)


data Function
  = SUM
  | AVG
  | COUNT
  deriving (Generic, Show)

instance GQLType Function where
  type KIND Function = ENUM


data LogicOperator
  = AND
  | OR
  | NOT

{-
instance FromJSON SemQuery where
  parseJSON (Object v) =
     SemQuery <$> v .: "command"
         <*> v .: "from"
         <*> v .: "target"
         <*> v .: "where"
  parseJSON _ = mzero

instance ToJSON SemQuery where
  toJSON (SemQuery command' fromClause' targetList' whereClause') =
    object [ "command" .= command'
           , "from"    .= fromClause'
           , "target"  .= targetList'
           , "where"   .= whereClause'
           ]

instance FromJSON Target where
  parseJSON (Object v) =
    Target <$> v .: "function"
           <*> v .: "target"
  parseJSON _ = mzero

instance ToJSON Target where
  toJSON (Target function' target') =
    object [ "function" .= function'
           , "target"   .= target'
           ]

instance FromJSON Function where
  parseJSON "sum"   = pure Sum
  parseJSON "avg"   = pure Avg
  parseJSON "count" = pure Count
  parseJSON _       = mzero

instance ToJSON Function where
  toJSON Sum   = "sum"
  toJSON Avg   = "avg"
  toJSON Count = "count"
-}
