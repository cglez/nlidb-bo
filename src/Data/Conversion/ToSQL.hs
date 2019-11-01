{-# LANGUAGE OverloadedStrings #-}

module Data.Conversion.ToSQL where

import           Data.String                (fromString)
import           Data.Text                  (Text, intercalate, splitOn)
import           Data.Maybe                 (fromMaybe)
import           Data.SemQuery              (SemQuery(..), Target(..), Function(..), Condition(..))


class ToSQL a where
  toSql :: a -> SQL

type SQL = Text

instance ToSQL Text where
  toSql = id

instance ToSQL a => ToSQL [a] where
  toSql xs = intercalate ", " $ map toSql xs

instance ToSQL a => ToSQL (Maybe a) where
  toSql (Just x) = toSql x
  toSql Nothing  = ""

instance ToSQL SemQuery where
  toSql (SemQuery command' targets' conditions') =
    command' <> " " <> toSql targets' <> " FROM " <> resolveRange targets' <> whereClause conditions'

instance ToSQL Target where
  toSql Target
    { function = func
    , argument = arg
    }
    = func <> "(" <> columnOf path <> ")"
    where
      path = splitOn "." $ fromMaybe "" arg

instance ToSQL Function where
  toSql = fromString . show

columnOf :: [Text] -> Text
columnOf [_] = "*"
columnOf [_, x] = x
columnOf [_, _, x] = x
columnOf _ = ""

tableOf :: [Text] -> Text
tableOf [x] = x
tableOf [x, _] = x
tableOf [_, x, _] = x
tableOf _ = ""

resolveRange :: [Target] -> Text
resolveRange (Target{ argument=arg }:_) =
  tableOf path
  where
    path = splitOn "." $ fromMaybe "" arg
resolveRange _ = ""


whereClause :: [Condition] -> Text
whereClause [] = ""
whereClause xs = " WHERE " <> toSql xs

instance ToSQL Condition where
  toSql Condition
    { subject = subj
    , operator = op
    , value = val
    }
    = subj <> operatorToSql op <> val

operatorToSql :: Text -> Text
operatorToSql "EQ" = "="
operatorToSql "NEQ" = "<>"
