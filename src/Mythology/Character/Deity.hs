{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Mythology.Character.Deity
  ( Deity(..)
  , dbDeity
  ) where

import           Data.Morpheus.Kind     (KIND, OBJECT)
import           Data.Morpheus.Types    (GQLType (..))
import           Data.Text              (Text, pack)
import           GHC.Generics           (Generic)
import           Mythology.Place.Places (Realm (..))
import           Database.Access        (query)

type instance KIND Deity = OBJECT

instance GQLType Deity where
  description _ = "Custom Description for Client Defined User Type"

data Deity = Deity
  { fullName :: Text -- Non-Nullable Field
  , power    :: Maybe Text -- Nullable Field
  , realm    :: Realm
  } deriving (Generic)

dbDeity :: Text -> Maybe Text -> IO (Either String Deity)
dbDeity _ _ = do
  name <- query
  return $ Right $ Deity {fullName = pack name, power = Just "Shapeshifting", realm = Dream}
