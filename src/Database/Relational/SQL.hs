module Database.Relational.SQL
  ( query
  ) where

import           Database.HDBC                 (SqlValue, quickQuery)
import           Database.HDBC.ODBC            (connectODBC)
import           Configuration.Environment     (getConf)


defaultType :: String
defaultType = ""
defaultPort :: String
defaultPort = "4000"
defaultPass :: String
defaultPass = "1234"

-- TODO: handle errors and return IO (Either String [[SqlValue]])
query :: String -> IO [[SqlValue]]
query sql = do
    connStr <- getOdbcConnStr
    conn <- connectODBC connStr
    quickQuery conn sql []

getOdbcConnStr :: IO String
getOdbcConnStr = do
    t    <- getConf defaultType "DB_TYPE"
    port <- getConf defaultPort "DB_PORT"
    pass <- getConf defaultPass "DB_PASS"
    let connStr = "Driver=" ++ t ++ ";Server=127.0.0.1;Port=" ++ port ++ ";"
    if t == "mariadb" || t == "mysql" then do
        name <- getConf "test" "DB_NAME"
        user <- getConf "root" "DB_USER"
        return $ connStr ++ "User=" ++ user ++ ";Password=" ++ pass ++ ";Database=" ++ name ++ ";"
    else if t == "postgres" then do
        user <- getConf "postgres" "DB_USER"
        return $ connStr ++ "UID=" ++ user ++ ";PWD=" ++ pass ++ ";"
    else
        return ""
