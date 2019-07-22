{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import           Control.Monad.IO.Class         (liftIO)
import           Configuration.Dotenv           (loadFile, defaultConfig)
import           Web.Scotty                     (body, get, post, raw, file, scotty)
import           SQL.API                        (sqlApi)

main :: IO ()
main = do
  _ <- loadFile defaultConfig
  scotty 3000 $ do
    get  "/" $ file "app/static/index.html"
    post "/" $ raw =<< (liftIO . sqlApi =<< body)
