module GraphQL.Resolvers
  ( rootResolver
  ) where

import qualified Data.ByteString.Lazy          as LBS (toStrict)
import           Data.Text                     (Text, unpack)
import           Data.Text.Encoding            (decodeUtf8)
import           Data.Aeson                    (encode)
import           Data.Morpheus.Types           (IORes, resolver, GQLRootResolver(..))
import           Data.Conversion.ToSQL         (SQL, toSql)
import           Data.SemQuery                 (SemQuery)
import           Data.SqlJSON                  (toJSON, SqlJSON(..))
import qualified Database.Relational.SQL       as DB (query)
import           GraphQL.Schema                (Query(..), SqlArgs(..), NlidbArgs(..))


rootResolver :: GQLRootResolver IO () () Query () ()
rootResolver =
  GQLRootResolver
    { queryResolver = return Query
      { sql = resolveSql
      , nlidb = resolveNlidb
      }
    , mutationResolver = return ()
    , subscriptionResolver = return ()
    }


resolveNlidb :: NlidbArgs -> IORes Text
resolveNlidb = resolver . nlidbBackend . semQuery

nlidbBackend :: SemQuery -> IO (Either String Text)
nlidbBackend semQuery' = do
  let sql' = toSql semQuery'
  print sql'
  sqlBackend sql'

resolveSql :: SqlArgs -> IORes Text
resolveSql = resolver . sqlBackend . GraphQL.Schema.query

sqlBackend :: SQL -> IO (Either String Text)
sqlBackend sql' = do
  result <- DB.query $ unpack sql'
  return . Right . decodeUtf8 . LBS.toStrict . encode . toJSON . map (map SqlJSON) $ result
  --return . Right $ map (map SqlJSON) result
