{-# LANGUAGE DeriveGeneric, RankNTypes, TypeOperators #-}

module GraphQL.Sql.API
  ( sqlApi
  ) where

import           GHC.Generics                  (Generic)
import qualified Data.ByteString.Lazy          as LB (ByteString, toStrict)
import           Data.Text                     (Text, unpack)
import           Data.Text.Encoding            (decodeUtf8)
import           Data.Aeson                    (encode)
import           Data.Morpheus                 (interpreter)
import           Data.Morpheus.Types           (GQLRootResolver(..), ResM, gqlResolver)
import           Database.HDBC.SqlJSON         (SqlJSON(..), toJSON)
import           Database.Relational.Sql       (query)

newtype Query = Query
  { sql :: SqlArgs -> ResM Text
  } deriving (Generic)

newtype SqlArgs = SqlArgs
  { expr :: Text
  } deriving (Generic)

sqlBackend :: Text -> IO (Either String Text)
sqlBackend sqlQuery = do
  result <- query . unpack $ sqlQuery
  let jsql = map (map SqlJSON) result
  return . Right . decodeUtf8 . LB.toStrict . encode . toJSON $ jsql

resolveSql :: SqlArgs -> ResM Text
resolveSql args = gqlResolver . sqlBackend $ expr args

rootResolver :: GQLRootResolver IO Query () ()
rootResolver =
  GQLRootResolver
    { queryResolver = return Query { sql = resolveSql }
    , mutationResolver = return ()
    , subscriptionResolver = return ()
    }

sqlApi :: LB.ByteString -> IO LB.ByteString
sqlApi = interpreter rootResolver
