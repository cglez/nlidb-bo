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
getConf x x0 = do
    y <- lookupEnv x
    return $ fromMaybe x0 y

query :: IO String
query = do
    db    <- getConf "DB_NAME" "test"
    port  <- getConf "DB_PORT" "3306"
    user  <- getConf "DB_USER" "root"
    pass  <- getConf "DB_PASS" "1234"
    sql   <- getConf "query" ""
    let cdn = "Driver={MySQL Unicode};Server=127.0.0.1;Port=" ++ port ++ ";" ++
              "User=" ++ user ++ ";Password=" ++ pass ++ ";Database=" ++ db ++";"
    conn <- connectODBC cdn
    rows <- quickQuery conn sql []
    return $ toJson rows
