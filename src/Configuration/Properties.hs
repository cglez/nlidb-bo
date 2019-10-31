module Configuration.Properties
  ( getProperty
  ) where

import Data.List (isPrefixOf, stripPrefix)


getConfigProperty :: String -> String -> Maybe String
getConfigProperty configFile k =
  let prefix = k ++ "="
      pairs = reverse $ filter (isPrefixOf prefix) $ lines configFile
  in case pairs of
    x:_ -> stripPrefix prefix x
    []  -> Nothing

getProperty :: FilePath -> String -> IO (Maybe String)
getProperty path k = do
  config <- readFile path
  return $ getConfigProperty config k
