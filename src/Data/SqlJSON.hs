{-# LANGUAGE TypeFamilies #-}

module Data.SqlJSON
  ( SqlJSON(..)
  , toJSON
  ) where

import           Data.Text.Encoding            (decodeLatin1)
import           Data.Aeson                    (ToJSON, toJSON, Value(Null))
import           Database.HDBC                 (SqlValue(..))


newtype SqlJSON = SqlJSON SqlValue

instance ToJSON SqlJSON where
  toJSON (SqlJSON (SqlString x))                 = toJSON x
  toJSON (SqlJSON (SqlByteString x))             = toJSON . decodeLatin1 $ x
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

{-
instance GQLType SqlJSON where
  type KIND SqlJSON = SCALAR

instance GQLScalar SqlJSON where
  parseValue _          = Left ""
  
  serialize (SqlJSON (SqlString x))                 = String . pack $ x
  serialize (SqlJSON (SqlByteString x))             = String . decodeLatin1 $ x
  serialize (SqlJSON (SqlInt32 x))                  = Int . fromIntegral $ x
  serialize (SqlJSON (SqlInt64 x))                  = Int . fromIntegral $ x
  serialize (SqlJSON (SqlWord32 x))                 = undefined --serialize x
  serialize (SqlJSON (SqlWord64 x))                 = undefined --serialize x
  serialize (SqlJSON (SqlInteger x))                = Int . fromIntegral $ x
  serialize (SqlJSON (SqlChar x))                   = String . singleton $ x
  serialize (SqlJSON (SqlBool x))                   = Boolean x
  serialize (SqlJSON (SqlDouble x))                 = undefined --serialize x
  serialize (SqlJSON (SqlRational x))               = undefined --serialize x
  serialize (SqlJSON (SqlLocalDate x))              = undefined --serialize x
  serialize (SqlJSON (SqlLocalTimeOfDay x))         = undefined --serialize x
  serialize (SqlJSON (SqlZonedLocalTimeOfDay _ _))  = undefined
  serialize (SqlJSON (SqlLocalTime _))              = undefined
  serialize (SqlJSON (SqlZonedTime x))              = undefined --serialize x
  serialize (SqlJSON (SqlUTCTime x))                = undefined --serialize x
  serialize (SqlJSON (SqlDiffTime x))               = undefined --serialize x
  serialize (SqlJSON (SqlPOSIXTime x))              = undefined --serialize x
  serialize (SqlJSON (SqlEpochTime x))              = undefined --serialize x
  serialize (SqlJSON (SqlTimeDiff x))               = undefined --serialize x
  serialize (SqlJSON SqlNull)                       = serialize . absurd ()
-}
