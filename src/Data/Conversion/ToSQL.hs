{-# LANGUAGE OverloadedStrings #-}

module Data.Conversion.ToSQL where

import           Data.String                (fromString)
import           Data.Text                  (Text, intercalate, splitOn)
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
  toSql (SemQuery command' targets' conditions') =
    command' <> " " <> toSql targets' <> " FROM " <> resolveRange targets' <> toSql (fromMaybe "" conditions')

instance ToSQL Target where
  toSql Target
    { function=f
    , argument=arg
    } = f <> "(" <> columnOf path <> ")"
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
