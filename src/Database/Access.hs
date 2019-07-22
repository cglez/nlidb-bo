module Database.Access
  ( query
  ) where

import           Data.Maybe                    (fromMaybe)
import           System.Environment            (lookupEnv)
import           Database.HDBC                 (SqlValue, quickQuery, fromSql)
import           Database.HDBC.ODBC            (connectODBC)

toJson :: [[SqlValue]] -> String
toJson table =
  let table' = map (map fromSql) table :: [[String]]
  in show table'

getConf :: String -> String -> IO String
getConf v0 k = do
    v <- lookupEnv k
    return $ fromMaybe v0 v

query :: String -> IO String
query sql = do
    db    <- getConf "test" "DB_NAME"
    port  <- getConf "3306" "DB_PORT"
    user  <- getConf "root" "DB_USER"
    pass  <- getConf "1234" "DB_PASS"
    let cdn = "Driver={MySQL Unicode};Server=127.0.0.1;Port=" ++ port ++ ";" ++
              "User=" ++ user ++ ";Password=" ++ pass ++ ";Database=" ++ db ++ ";"
    conn <- connectODBC cdn
    rows <- quickQuery conn sql []
    return $ toJson rows
