module Database.Access
  ( query
  ) where

import           Data.Maybe                    (fromMaybe)
import qualified Data.ByteString.UTF8 as B     (toString)
import           System.Environment            (lookupEnv)
import           Database.HDBC                 (SqlValue(..), quickQuery)
import           Database.HDBC.ODBC            (connectODBC)
import           Data.Aeson                    (ToJSON, toJSON, Value(..))

instance ToJSON SqlValue where
  toJSON (SqlString x)                 = toJSON x
  toJSON (SqlByteString x)             = toJSON . B.toString $ x
  toJSON (SqlInt32 x)                  = toJSON x
  toJSON (SqlInt64 x)                  = toJSON x
  toJSON (SqlWord32 x)                 = toJSON x
  toJSON (SqlWord64 x)                 = toJSON x
  toJSON (SqlInteger x)                = toJSON x
  toJSON (SqlChar x)                   = toJSON x
  toJSON (SqlBool x)                   = toJSON x
  toJSON (SqlDouble x)                 = toJSON x
  toJSON (SqlRational x)               = toJSON x
  toJSON (SqlLocalDate x)              = toJSON x
  toJSON (SqlLocalTimeOfDay x)         = toJSON x
  toJSON (SqlZonedLocalTimeOfDay _ _)  = Null
  toJSON (SqlLocalTime _)              = Null
  toJSON (SqlZonedTime x)              = toJSON x
  toJSON (SqlUTCTime x)                = toJSON x
  toJSON (SqlDiffTime x)               = toJSON x
  toJSON (SqlPOSIXTime x)              = toJSON x
  toJSON (SqlEpochTime x)              = toJSON x
  toJSON (SqlTimeDiff x)               = toJSON x
  toJSON SqlNull                       = Null

getConf :: String -> String -> IO String
getConf v0 k = do
    v <- lookupEnv k
    return $ fromMaybe v0 v

-- TODO: handle errors and return IO (Either String [[SqlValue]])
query :: String -> IO [[SqlValue]]
query sql = do
    db    <- getConf "test" "DB_NAME"
    port  <- getConf "3306" "DB_PORT"
    user  <- getConf "root" "DB_USER"
    pass  <- getConf "1234" "DB_PASS"
    let cdn = "Driver={MySQL Unicode};Server=127.0.0.1;Port=" ++ port ++ ";" ++
              "User=" ++ user ++ ";Password=" ++ pass ++ ";Database=" ++ db ++ ";"
    conn <- connectODBC cdn
    quickQuery conn sql []
