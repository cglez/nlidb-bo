module Database.Relational.SQL
  ( query
  ) where

import           Database.HDBC                 (SqlValue, quickQuery)
import           Database.HDBC.ODBC            (connectODBC)
import           Configuration.Environment     (getConf)


-- TODO: handle errors and return IO (Either String [[SqlValue]])
query :: String -> IO [[SqlValue]]
query sql = do
    cdn <- getOdbcCdn
    conn <- connectODBC cdn
    quickQuery conn sql []

getOdbcCdn :: IO String
getOdbcCdn = do
    t    <- getConf ""     "DB_TYPE"
    print $ "DB_TYPE: " ++ t
    port <- getConf "4000" "DB_PORT"
    pass <- getConf "1234" "DB_PASS"
    let cdn = "Driver=" ++ t ++ ";Server=127.0.0.1;Port=" ++ port ++ ";"
    if t == "mariadb" || t == "mysql" then do
        name <- getConf "test" "DB_NAME"
        user <- getConf "root" "DB_USER"
        return $ cdn ++ "User=" ++ user ++ ";Password=" ++ pass ++ ";Database=" ++ name ++ ";"
    else if t == "postgres" then do
        user <- getConf "postgres" "DB_USER"
        return $ cdn ++ "UID=" ++ user ++ ";PWD=" ++ pass ++ ";"
    else
        return ""
