{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Database.HDBC.SqlJSON
  ( SqlJSON(..)
  , toJSON
  ) where

import           Data.Aeson                    (ToJSON, toJSON, Value(Null))
import           Database.HDBC                 (SqlValue(..))
import           GHC.Generics                  (Generic)
import           Data.Text                     (pack, Text)
import           Data.Text.Encoding            (decodeLatin1)
import           Data.Morpheus.Types           (GQLType, GQLScalar(..), ScalarValue(..))
import           Data.Morpheus.Kind            (KIND, SCALAR)
import Data.Aeson.Types (Value(Bool))
import GHC.Float (double2Float)
--import           Data.ByteString.Conversion    (ToByteString)

instance GQLType SqlValue

newtype SqlJSON = SqlJSON SqlValue deriving (Generic, GQLType)

instance GQLScalar SqlJSON where
  parseValue = undefined
  --serialize (SqlJSON x) = String . show $ x
  serialize (SqlJSON (SqlString x))                 = String $ pack x
  serialize (SqlJSON (SqlByteString x))             = String $ decodeLatin1 x
  serialize (SqlJSON (SqlInt32 x))                  = Int $ fromIntegral x
  serialize (SqlJSON (SqlInt64 x))                  = Int $ fromIntegral x
  --serialize (SqlJSON (SqlWord32 x))                 = String . ToByteString $ x
  --serialize (SqlJSON (SqlWord64 x))                 = serialize x
  serialize (SqlJSON (SqlInteger x))                = Int $ fromIntegral x
  serialize (SqlJSON (SqlChar x))                   = String $ pack [x]
  serialize (SqlJSON (SqlBool x))                   = Boolean x
  serialize (SqlJSON (SqlDouble x))                 = Float $ double2Float x
  serialize (SqlJSON (SqlRational x))               = Float $ realToFrac x
  --serialize (SqlJSON (SqlLocalDate x))              = serialize x
  --serialize (SqlJSON (SqlLocalTimeOfDay x))         = serialize x
  serialize (SqlJSON (SqlZonedLocalTimeOfDay _ _))  = undefined
  serialize (SqlJSON (SqlLocalTime _))              = undefined
  --serialize (SqlJSON (SqlZonedTime x))              = serialize x
  --serialize (SqlJSON (SqlUTCTime x))                = serialize x
  --serialize (SqlJSON (SqlDiffTime x))               = serialize x
  --serialize (SqlJSON (SqlPOSIXTime x))              = serialize x
  --serialize (SqlJSON (SqlEpochTime x))              = serialize x
  --serialize (SqlJSON (SqlTimeDiff x))               = serialize x
  serialize (SqlJSON SqlNull)                       = String $ pack "" --replaceValue Null --Null

type instance KIND SqlJSON = SCALAR

instance ToJSON SqlJSON where
  toJSON (SqlJSON (SqlString x))                 = toJSON x
  toJSON (SqlJSON (SqlByteString x))             = toJSON $ decodeLatin1 x
  toJSON (SqlJSON (SqlInt32 x))                  = toJSON x
  toJSON (SqlJSON (SqlInt64 x))                  = toJSON x
  toJSON (SqlJSON (SqlWord32 x))                 = toJSON x
  toJSON (SqlJSON (SqlWord64 x))                 = toJSON x
  toJSON (SqlJSON (SqlInteger x))                = toJSON x
  toJSON (SqlJSON (SqlChar x))                   = toJSON x
  toJSON (SqlJSON (SqlBool x))                   = toJSON x
  toJSON (SqlJSON (SqlDouble x))                 = toJSON x
  toJSON (SqlJSON (SqlRational x))               = toJSON x
  toJSON (SqlJSON (SqlLocalDate x))              = toJSON x
  toJSON (SqlJSON (SqlLocalTimeOfDay x))         = toJSON x
  toJSON (SqlJSON (SqlZonedLocalTimeOfDay _ _))  = undefined
  toJSON (SqlJSON (SqlLocalTime _))              = undefined
  toJSON (SqlJSON (SqlZonedTime x))              = toJSON x
  toJSON (SqlJSON (SqlUTCTime x))                = toJSON x
  toJSON (SqlJSON (SqlDiffTime x))               = toJSON x
  toJSON (SqlJSON (SqlPOSIXTime x))              = toJSON x
  toJSON (SqlJSON (SqlEpochTime x))              = toJSON x
  toJSON (SqlJSON (SqlTimeDiff x))               = toJSON x
  toJSON (SqlJSON SqlNull)                       = Null
