module Database.Relational.Sql
  ( query
  ) where

import           Database.HDBC                 (SqlValue, quickQuery)
import           Database.HDBC.ODBC            (connectODBC)
import           Configuration.Environment     (getConf)


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
