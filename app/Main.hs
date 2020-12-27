module Main
  ( main
  ) where

import           Control.Monad.IO.Class         (liftIO)
import           Web.Scotty                     (body, get, post, raw, file, param, scotty)
import           GraphQL.API                    (gqlApi)


main :: IO ()
main =
  scotty 3000 $ do
    get  "/" $ file "app/static/index.html"
    post "/" $ raw =<< (liftIO . gqlApi =<< body)
    get  "/:dir/:f" $ do
      dir <- param "dir"
      f <- param "f"
      file $ "app/static/" <> dir <> "/" <> f
