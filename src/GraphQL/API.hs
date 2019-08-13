{-# LANGUAGE DeriveGeneric, RankNTypes, TypeOperators #-}

module GraphQL.API
  ( gqlApi
  ) where

import           GHC.Generics                  (Generic)
import qualified Data.ByteString.Lazy          as LBS (ByteString, fromStrict, toStrict)
import           Data.Text                     (Text, empty, unpack)
import           Data.Text.Encoding            (decodeUtf8, encodeUtf8)
import           Data.Aeson                    (encode, decode)
import           Data.Morpheus                 (interpreter)
import           Data.Morpheus.Types           (GQLRootResolver(..), ResM, gqlResolver)
import           Database.HDBC.SqlJSON         (SqlJSON(..), toJSON)
import           Database.Relational.Sql       (query)
import           AST.Generator                 (toSql)


data Query = Query
  { raw :: SqlArgs -> ResM Text
  , nli :: NliArgs -> ResM Text
  } deriving (Generic)

newtype SqlArgs = SqlArgs
  { sql :: Text
  } deriving (Generic)

newtype NliArgs = NliArgs
  { ast :: Text
  } deriving (Generic)

sqlBackend :: Text -> IO (Either String Text)
sqlBackend sqlQuery = do
  result <- query . unpack $ sqlQuery
  let jsql = map (map SqlJSON) result
  return . Right . decodeUtf8 . LBS.toStrict . encode . toJSON $ jsql

nliBackend :: Text -> IO (Either String Text)
nliBackend astExpr = sqlBackend sqlQuery
  where select = decode . LBS.fromStrict . encodeUtf8 $ astExpr
        sqlQuery = case select of
          Just x  -> toSql x
          Nothing -> empty

resolveSql :: SqlArgs -> ResM Text
resolveSql = gqlResolver . sqlBackend . sql

resolveNli :: NliArgs -> ResM Text
resolveNli = gqlResolver . nliBackend . ast

rootResolver :: GQLRootResolver IO Query () ()
rootResolver =
  GQLRootResolver
    { queryResolver = return Query { raw = resolveSql, nli = resolveNli }
    , mutationResolver = return ()
    , subscriptionResolver = return ()
    }

gqlApi :: LBS.ByteString -> IO LBS.ByteString
gqlApi = interpreter rootResolver
