{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Conversion.ToSQL where

import           Data.String                (fromString)
import           Data.Text                  (Text, intercalate, splitOn)
import           Data.List                  (nub)
import           Data.Tuple                 (swap)
import           Data.Maybe                 (fromMaybe)
import           Data.SemQuery              (SemQuery(..), Target(..), Function(..), Condition(..))


type SQL = Text

class ToSQL a where
  toSql :: a -> SQL

instance ToSQL SQL where
  toSql = id

instance ToSQL [Target] where
  toSql xs = intercalate ", " $ map toSql xs

instance ToSQL a => ToSQL (Maybe a) where
  toSql (Just x) = toSql x
  toSql Nothing  = ""

instance ToSQL SemQuery where
  toSql (SemQuery command' targets' conditions') =
    command' <> " " <> toSql targets' <> resolveRange targets' conditions' <> toSql conditions'

instance ToSQL Target where
  toSql Target
    { function = func
    , argument = arg
    }
    = func <> "(" <> columnOf path <> ")"
    where path = pathOf $ fromMaybe "" arg

instance ToSQL Function where
  toSql = fromString . show

pathOf :: Text -> [Text]
pathOf = splitOn "."

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

resolveRange :: [Target] -> [Condition] -> Text
resolveRange [] _ = ""
resolveRange xs ys =
  let tables = findTables xs ys
      joins = resolveJoins relationsEuro tables
  in
    " FROM " <> head tables <> joins

findTables :: [Target] -> [Condition] -> [Text]
findTables x = nub . findTables' x

findTables' :: [Target] -> [Condition] -> [Text]
findTables' (Target{argument=arg} : xs) ys =
  let table = (tableOf . pathOf . fromMaybe "") arg
  in table : findTables' xs ys
findTables' [] (Condition{subject=arg} : ys) =
  let table = (tableOf . pathOf) arg
  in table : findTables' [] ys
findTables' [] [] = []

type Relation = (Text, Text)
type RelationGraph = [Relation]
type RelationPath = [Relation]

resolveJoins :: RelationGraph -> [Text] -> Text
resolveJoins graph (x:xs) = foldr ((<>) . joinToSql graph x) "" xs
resolveJoins _ [] = ""

joinToSql :: RelationGraph -> Text -> Text -> Text
joinToSql graph x y =
  let relation = findRelation graph x y
  in case relation of
    Just (a, b) -> " JOIN " <> (tableOf . pathOf) a <> " ON " <> a <> "=" <> b
    Nothing -> ""

head' :: [a] -> Maybe a
head' []    = Nothing
head' (x:_) = Just x

findRelation :: RelationGraph -> Text -> Text -> Maybe Relation
findRelation graph x y =
  head' $ filter (\(a, b) ->
    (tableOf . pathOf) a == y && (tableOf . pathOf) b == x)
    $ graph ++ map swap graph

instance ToSQL [Condition] where
  toSql [] = ""
  toSql xs = " WHERE " <> intercalate " AND " (map toSql xs)

instance ToSQL Condition where
  toSql Condition
    { subject = subj
    , operator = op
    , value = val
    }
    = subjectOf (pathOf subj) <> operatorToSql op <> val

subjectOf :: [Text] -> Text
subjectOf x = subjectOf' (tableOf x) (columnOf x)

subjectOf' :: Text -> Text -> Text
subjectOf' "" x = x
subjectOf' x "" = x
subjectOf' x y  = x <> "." <> y

operatorToSql :: Text -> Text
operatorToSql "EQ"  = "="
operatorToSql "NE"  = "<>"
operatorToSql "GT"  = ">"
operatorToSql "GTE" = ">="
operatorToSql "LT"  = "<"
operatorToSql "LTE" = "<="
operatorToSql _     = ""

relationsEuro :: RelationGraph
relationsEuro = [
  ("asst_referee_mast.country_id", "soccer_country.country_id"),
  ("goal_details.match_no", "match_mast.match_no"),
  ("goal_details.player_id", "player_mast.player_id"),
  ("goal_details.team_id", "soccer_country.country_id"),
  ("match_captain.match_no", "match_mast.match_no"),
  ("match_captain.player_captain", "player_mast.player_id"),
  ("match_captain.team_id", "soccer_country.country_id"),
  ("match_details.ass_ref", "asst_referee_mast.ass_ref_id"),
  ("match_details.match_no", "match_mast.match_no"),
  ("match_details.player_gk", "player_mast.player_id"),
  ("match_details.team_id", "soccer_country.country_id"),
  ("match_mast.plr_of_match", "player_mast.player_id"),
  ("match_mast.referee_id", "referee_mast.referee_id"),
  ("match_mast.venue_id", "soccer_venue.venue_id"),
  ("penalty_gk.match_no", "match_mast.match_no"),
  ("penalty_gk.player_gk", "player_mast.player_id"),
  ("penalty_gk.team_id", "soccer_country.country_id"),
  ("penalty_shootout.match_no", "match_mast.match_no"),
  ("penalty_shootout.player_id", "player_mast.player_id"),
  ("penalty_shootout.team_id", "soccer_country.country_id"),
  ("player_booked.match_no", "match_mast.match_no"),
  ("player_booked.player_id", "player_mast.player_id"),
  ("player_booked.team_id", "soccer_country.country_id"),
  ("player_in_out.match_no", "match_mast.match_no"),
  ("player_in_out.player_id", "player_mast.player_id"),
  ("player_in_out.team_id", "soccer_country.country_id"),
  ("player_mast.posi_to_play", "playing_position.position_id"),
  ("player_mast.team_id", "soccer_country.country_id"),
  ("referee_mast.country_id", "soccer_country.country_id"),
  ("soccer_city.country_id", "soccer_country.country_id"),
  ("soccer_team.team_id", "soccer_country.country_id"),
  ("soccer_venue.city_id", "soccer_city.city_id"),
  ("team_coaches.coach_id", "coach_mast.coach_id"),
  ("team_coaches.team_id", "soccer_country.country_id")]
