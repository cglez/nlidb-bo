{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TypeOperators #-}

module SQL.API where

import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Morpheus              (interpreter)
import           Data.Morpheus.Types        (GQLRootResolver (..), ResM, gqlResolver)
import           Data.Text                  (Text, pack, unpack)
import           GHC.Generics               (Generic)
import           Database.Access            (query)

newtype Query = Query
  { sql :: SqlArgs -> ResM Text
  } deriving (Generic)

newtype SqlArgs = SqlArgs
  { expr :: Text
  } deriving (Generic)

sqlBackend :: Text -> IO (Either String Text)
sqlBackend sqlQuery = do
  result <- query $ unpack sqlQuery
  return $ Right $ pack result

resolveSql :: SqlArgs -> ResM Text
resolveSql args = gqlResolver $ sqlBackend (expr args)

rootResolver :: GQLRootResolver IO Query () ()
rootResolver =
  GQLRootResolver
    { queryResolver = return Query {sql = resolveSql}
    , mutationResolver = return ()
    , subscriptionResolver = return ()
    }

sqlApi :: B.ByteString -> IO B.ByteString
sqlApi = interpreter rootResolver
