module Database.HDBC.SqlJSON
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
