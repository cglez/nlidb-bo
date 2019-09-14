module GraphQL.API
  ( gqlApi
  ) where

import qualified Data.ByteString.Lazy          as LBS (ByteString)
import           Data.Morpheus                 (interpreter)
import           GraphQL.Resolvers             (rootResolver)


gqlApi :: LBS.ByteString -> IO LBS.ByteString
gqlApi = interpreter rootResolver
