module Database.Relational.SQL
  ( query
  ) where

import           Control.Exception.Base        (try)
import           Database.HDBC                 (SqlValue, quickQuery, handleSqlError)
import           Database.HDBC.ODBC            (connectODBC)
import           Configuration.Environment     (getConf)


defaultType :: String
defaultType = ""
defaultPort :: String
defaultPort = "4000"
defaultPass :: String
defaultPass = "1234"

query :: String -> IO (Either String [[SqlValue]])
query sql = do
    connStr <- getOdbcConnStr
    conn <- connectODBC connStr
    res <- try $ handleSqlError $ quickQuery conn sql []
    return $ case (res::Either IOError [[SqlValue]]) of
      Right x -> Right x
      Left  x -> Left . show $ x

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
