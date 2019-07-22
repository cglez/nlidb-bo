module Database.Access
    ( query
    ) where

import Database.HDBC (SqlValue, quickQuery, fromSql)
import Database.HDBC.ODBC (connectODBC)
import EnvFile (getOrDef)

toJson :: [[SqlValue]] -> String
toJson table =
  let table' = map (map fromSql) table :: [[String]]
  in show table'

query :: IO String
query = do
    let getConf = getOrDef ".env"
    db    <- getConf "DB_NAME" "test"
    port  <- getConf "DB_PORT" "3306"
    user  <- getConf "DB_USER" "root"
    pass  <- getConf "DB_PASS" "1234"
    query <- getConf "query" ""
    let cdn = "Driver={MySQL Unicode};Server=127.0.0.1;Port=" ++ port ++ ";" ++
              "User=" ++ user ++ ";Password=" ++ pass ++ ";Database=" ++ db ++";"
    conn <- connectODBC cdn
    rows <- quickQuery conn query []
    return $ toJson rows
