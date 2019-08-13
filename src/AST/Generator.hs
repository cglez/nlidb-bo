{-# LANGUAGE OverloadedStrings #-}

module AST.Generator where

import           Data.Text                  (Text, intercalate)
import           AST.Parser                 (Select(..), Target(..), target, function, Function(..))
import           Data.Maybe                 (fromMaybe)


toSql :: Select -> Text
toSql (Select _ fromClause' targetList' whereClause') =
  "SELECT " <> targetToSql targetList' <> " FROM " <> fromClauseToSql fromClause' <> fromMaybe "" whereClause'

targetToSql :: [Target] -> Text
targetToSql xs = intercalate ", " xs'
  where xs' = map toString xs :: [Text]
        toString Target{function=f, target=t} = functionName f <> "(" <> fromMaybe "*" t <> ") " :: Text

functionName :: Function -> Text
functionName Sum   = "sum"
functionName Avg   = "avg"
functionName Count = "count"

fromClauseToSql :: Text -> Text
fromClauseToSql = id
