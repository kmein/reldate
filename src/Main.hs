module Main where

import Lib

import Data.Char (toLower)
import Data.Monoid ((<>))
import Data.Time (DayOfWeek(..), UTCTime(..), getCurrentTime)
import Options.Applicative

parseDayOfWeek :: String -> DayOfWeek
parseDayOfWeek d =
    case map toLower d of
        "mon" -> Monday
        "tue" -> Tuesday
        "wed" -> Wednesday
        "thu" -> Thursday
        "fri" -> Friday
        "sat" -> Saturday
        "sun" -> Sunday
        _ -> error $ "could not parse week day: " ++ d

setting :: Parser Setting
setting =
    hsubparser $
    mconcat
        [ command "week" $
          info
              (Week <$> day)
              (progDesc "Allows creation of week-relative date streams.")
        , command "month" $
          info
              (Month <$>
               option
                   auto
                   (long "ord" <> short 'o' <> metavar "N" <>
                    help "ord value of the repeated date") <*>
               day)
              (progDesc "Allows creation of month-relative date streams.")]
  where
    day =
        parseDayOfWeek <$>
        strOption
            (long "day" <> short 'd' <> metavar "mon|tue|..." <>
             help "day of week")

main :: IO ()
main = do
    act <- execParser opts
    today <- utctDay <$> getCurrentTime
    mapM_ print $ dates today act
  where
    opts =
        info
            (setting <**> helper)
            (fullDesc <> progDesc "Prints relative dates.")
