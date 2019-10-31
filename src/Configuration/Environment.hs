module Configuration.Environment
 ( getConf
 ) where

import           Data.Maybe                    (fromMaybe)
import           System.Environment            (lookupEnv)
import           Configuration.Properties      (getProperty)


getConf :: String -> String -> IO String
getConf v0 k = do
  envVal <- lookupEnv k
  dotEnvVal <- getProperty ".env" k
  return $ fromMaybe (fromMaybe v0 dotEnvVal) envVal
