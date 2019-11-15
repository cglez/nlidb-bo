{-# LANGUAGE OverloadedStrings #-}

module Data.Conversion.ToSQL where

import           Data.String                (fromString)
import           Data.Text                  (Text, intercalate, splitOn)
import           Data.Maybe                 (fromMaybe)
import           Data.SemQuery              (SemQuery(..), Target(..), Function(..), Condition(..))


type SQL = Text

class ToSQL a where
  toSql :: a -> SQL

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
whereClause xs = " WHERE " <> intercalate " AND " (map toSql xs)

instance ToSQL Condition where
  toSql Condition
    { subject = subj
    , operator = op
    , value = val
    }
    = subjectOf (splitOn "." subj) <> operatorToSql op <> val

subjectOf :: [Text] -> Text
subjectOf x = subjectToSql (tableOf x) (columnOf x)

subjectToSql :: Text -> Text -> Text
subjectToSql "" x = x
subjectToSql x "" = x
subjectToSql x y  = x <> "." <> y

operatorToSql :: Text -> Text
operatorToSql "EQ"  = "="
operatorToSql "NE"  = "<>"
operatorToSql "GT"  = ">"
operatorToSql "GTE" = ">="
operatorToSql "LT"  = "<"
operatorToSql "LTE" = "<="
operatorToSql _     = ""
