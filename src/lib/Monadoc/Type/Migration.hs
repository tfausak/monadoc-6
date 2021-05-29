module Monadoc.Type.Migration where

import Monadoc.Prelude

import qualified Data.Fixed as Fixed
import qualified Data.Time as Time
import qualified Database.SQLite.Simple as Sql

data Migration = Migration
    { sql :: Sql.Query
    , time :: Time.UTCTime
    } deriving (Eq, Show)

new
    :: Time.Year
    -> Time.MonthOfYear
    -> Time.DayOfMonth
    -> Int
    -> Int
    -> Fixed.Pico
    -> String
    -> Migration
new year month day hour minute second q = Migration
    { sql = into @Sql.Query q
    , time = Time.UTCTime (Time.fromGregorian year month day)
        <. Time.timeOfDayToTime
        <| Time.TimeOfDay hour minute second
    }
