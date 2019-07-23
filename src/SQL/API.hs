{-# LANGUAGE DeriveGeneric, RankNTypes, TypeOperators #-}

module SQL.API
  ( sqlApi
  ) where

import qualified Data.ByteString.Lazy.Char8 as B
                                            (ByteString, toStrict)
import           GHC.Generics               (Generic)
import           Data.Text                  (Text, unpack)
import           Data.Text.Encoding         (decodeUtf8)
import           Data.Aeson                 (toJSON, encode)
import           Data.Morpheus              (interpreter)
import           Data.Morpheus.Types        (GQLRootResolver(..), ResM, gqlResolver)
import           Database.Access            (query)

newtype Query = Query
  { sql :: SqlArgs -> ResM Text
  } deriving (Generic)

newtype SqlArgs = SqlArgs
  { expr :: Text
  } deriving (Generic)

-- TODO: properly handle non-ascii codification
sqlBackend :: Text -> IO (Either String Text)
sqlBackend sqlQuery = do
  result <- query . unpack $ sqlQuery
  return . Right . decodeUtf8 . B.toStrict . encode . toJSON $ result

resolveSql :: SqlArgs -> ResM Text
resolveSql args = gqlResolver . sqlBackend $ expr args

rootResolver :: GQLRootResolver IO Query () ()
rootResolver =
  GQLRootResolver
    { queryResolver = return Query {sql = resolveSql}
    , mutationResolver = return ()
    , subscriptionResolver = return ()
    }

sqlApi :: B.ByteString -> IO B.ByteString
sqlApi = interpreter rootResolver
