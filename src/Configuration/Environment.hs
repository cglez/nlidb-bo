module Configuration.Environment
 ( getConf
 ) where

import           Data.Maybe                    (fromMaybe)
import           System.Environment            (lookupEnv)


getConf :: String -> String -> IO String
getConf v0 k = do
    v <- lookupEnv k
    return $ fromMaybe v0 v
