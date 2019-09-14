{-# LANGUAGE OverloadedStrings #-}

module Data.Conversion.ToSQL where

import           Data.String                (fromString)
import           Data.Text                  (Text, intercalate)
import           Data.Maybe                 (fromMaybe)
import           Data.SemQuery              (SemQuery(..), Target(..), Function(..))


class ToSQL a where
  toSql :: a -> SQL

type SQL = Text

instance ToSQL Text where
  toSql = id

instance ToSQL a => ToSQL [a] where
  toSql xs = intercalate ", " $ map toSql xs

instance ToSQL SemQuery where
  toSql (SemQuery command' range' targets' conditions') =
    command' <> " " <> toSql targets' <> " FROM " <> toSql range' <> toSql (fromMaybe "" conditions')

instance ToSQL Target where
  toSql Target
    { function=f
    , argument=t
    } = f <> "(" <> fromMaybe "*" t <> ")"

instance ToSQL Function where
  toSql = fromString . show
