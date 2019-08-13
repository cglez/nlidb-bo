{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module AST.Parser where

import qualified Data.Text                   as TS (Text)
import           Data.Aeson                  --(FromJSON, ToJSON, parseJSON, toJSON)
import           Control.Monad               (mzero)
import           GHC.Generics                (Generic)


data Select =
    Select { stmtType    :: Maybe TS.Text
           , fromClause  :: TS.Text
           , targetList  :: [Target]
           , whereClause :: Maybe TS.Text
           }
    deriving (Show, Eq, Generic)

data Target =
    Target { function  :: Function
           , target    :: Maybe TS.Text
           }
    deriving (Show, Eq, Generic)

data Operator = Eq | Gt | Lt | Ne | Contains
data Function = Sum | Avg | Count deriving (Show, Eq, Generic)
data Name a = Relation a | Attr a
data Quantifier = All | Any | Each
data Logic = And | Or | Not

instance FromJSON Select where
  parseJSON (Object v) =
     Select <$> v .: "type"
            <*> v .: "from"
            <*> v .: "target"
            <*> v .: "where"
  parseJSON _ = mzero

instance ToJSON Select where
 toJSON (Select stmtType' fromClause' targetList' whereClause') =
    object [ "type"   .= stmtType'
           , "from"   .= fromClause'
           , "target" .= targetList'
           , "where"  .= whereClause'
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
