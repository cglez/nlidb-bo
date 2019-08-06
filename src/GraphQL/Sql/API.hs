{-# LANGUAGE DeriveGeneric, RankNTypes, TypeOperators #-}

module GraphQL.Sql.API
  ( sqlApi
  ) where

import           GHC.Generics                  (Generic)
import qualified Data.ByteString.Lazy          as LB (ByteString)
import           Data.Text                     (Text, unpack)
import           Data.Morpheus                 (interpreter)
import           Data.Morpheus.Types           (GQLRootResolver(..), ResM, gqlResolver)
import           Database.HDBC.SqlJSON         (SqlJSON(..))
import           Database.Relational.Sql       (query)

newtype Query = Query
  { table :: TableArgs -> ResM [[SqlJSON]]
  } deriving (Generic)

newtype TableArgs = TableArgs
  { expr :: Text
  } deriving (Generic)

sqlBackend :: Text -> IO (Either String [[SqlJSON]])
sqlBackend sqlQuery = do
  result <- query . unpack $ sqlQuery
  let jtable = map (map SqlJSON) result
  return . Right $ jtable

resolveTable :: TableArgs -> ResM [[SqlJSON]]
resolveTable args = gqlResolver . sqlBackend $ expr args

rootResolver :: GQLRootResolver IO Query () ()
rootResolver =
  GQLRootResolver
    { queryResolver = return Query { table = resolveTable }
    , mutationResolver = return ()
    , subscriptionResolver = return ()
    }

sqlApi :: LB.ByteString -> IO LB.ByteString
sqlApi = interpreter rootResolver
