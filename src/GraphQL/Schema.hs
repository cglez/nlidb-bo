{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

module GraphQL.Schema
  ( Query(..)
  , SqlArgs(..)
  , NlidbArgs(..)
  ) where

import           GHC.Generics                  (Generic)
import           Data.Text                     (Text)
import           Data.Morpheus.Types           (IORes)
import           Data.SemQuery                 (SemQuery)


data Query = Query
  { sql :: SqlArgs -> IORes Text
  , nlidb :: NlidbArgs -> IORes Text
  } deriving (Generic)

newtype NlidbArgs = NlidbArgs
  { semQuery :: SemQuery
  } deriving (Generic)

newtype SqlArgs = SqlArgs
  { query :: Text
  } deriving (Generic)
