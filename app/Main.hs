{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import           Control.Monad.IO.Class         (liftIO)
import           Mythology.API                  (mythologyApi)
import           Web.Scotty                     (body, get, post, raw, file, scotty)

main :: IO ()
main = scotty 3000 $ do
  get  "/" $ file "app/static/index.html"
  post "/" $ raw =<< (liftIO . mythologyApi =<< body)
