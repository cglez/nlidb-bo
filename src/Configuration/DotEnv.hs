module Configuration.DotEnv
  ( get
  , getOrDef
  ) where

import Data.List (isPrefixOf, stripPrefix)

getByKey :: String -> String -> Maybe String
getByKey config k =
  let prefix = k ++ "="
      pairs = reverse $ filter (isPrefixOf prefix) $ lines config
  in case pairs of
      x:_ -> stripPrefix prefix x
      []  -> Nothing

get :: FilePath -> String -> IO (Maybe String)
get path k =
  do config <- readFile path
     return $ getByKey config k

orDefault :: Maybe String -> String -> String
orDefault (Just v) _ = v
orDefault Nothing v0 = v0

getOrDef :: FilePath -> String -> String -> IO String
getOrDef path k d =
  do v <- get path k
     return $ orDefault v d
