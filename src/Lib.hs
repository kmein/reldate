module Lib where

import Data.List (groupBy)
import Data.Maybe (mapMaybe)
import Data.Time
import Safe (atMay)

month :: Int -> Integer -> [Day]
month m y = [fromGregorian y m 1 .. fromGregorian y m 31]
 -- 31 is automatically clipped to the last day of the month

weekDays :: DayOfWeek -> [Day] -> [Day]
weekDays dow = filter (\d -> dayOfWeek d == dow)

data Setting
    = Week DayOfWeek
    | Month Int
            DayOfWeek

dates :: Day -> Setting -> [Day]
dates today (Week d) = weekDays d [today ..]
dates today (Month o d) =
    mapMaybe (\m -> weekDays d m `atMay` (o - 1)) $ groupMonths [today ..]
  where
    groupMonths =
        groupBy $
        \d1 d2 ->
             let (y1, m1, _) = toGregorian d1
                 (y2, m2, _) = toGregorian d2
             in m1 == m2 && y1 == y2

