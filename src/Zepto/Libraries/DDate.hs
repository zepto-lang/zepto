module Zepto.Libraries.DDate where
import Control.Monad
import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.Calendar.OrdinalDate (toOrdinalDate)
import System.Random (randomRIO)

weekdays :: [String]
weekdays = [ "Setting Orange"
           , "Sweetmorn"
           , "Boomtime"
           , "Pungenday"
           , "Prickle-Prickle"
           ]

seasons :: [String]
seasons = [ "Chaos"
          , "Discord"
          , "Confusion"
          , "Bureacracy"
          , "The Aftermath"
          ]

fluxes :: [String]
fluxes = [ "Chaoflux"
         , "Discoflux"
         , "Confuflux"
         , "Bureflux"
         , "Afflux"
         ]

holydays :: [String]
holydays = [ "Mungday"
           , "Mojoday"
           , "Syaday"
           , "Zaraday"
           , "Maladay"
           ]

season :: Int -> String
season x = seasons !! quot x 73

weekday :: Int -> String
weekday x = weekdays !! rem x 5

monthday :: Int -> String
monthday x =
        if head day == '1' && length day == 2
            then day ++ "th"
            else case last day of
                '1' -> day ++ "st"
                '2' -> day ++ "nd"
                '3' -> day ++ "rd"
                _   -> day ++ "th"
    where day = show $ rem x 73

yold :: Integer -> String
yold year = show $ year + 1166

holyday :: Int -> String
holyday day
    | rem day 73 == 50 =
            printHoly holydays day
    | rem day 73 == 5 =
            printHoly fluxes day
    | otherwise = ""
    where printHoly l x = "\nCelebrate " ++ l !! quot x 73 ++ "!"

date :: (Integer, Int) -> String
date (year, day) =
        if isLeapYear year
            then if day > 60
                then
                    let x = day - 1
                    in printDDate (weekday x) (monthday x) (season x)
                                  (yold year) (holyday x)
                else if day < 60
                    then printDDate (weekday day) (monthday day) (season day)
                                    (yold year) (holyday day)
                    else "Today is St. Tib's Day in the YOLD " ++ yold year
            else printDDate (weekday day) (monthday day) (season day)
                            (yold year) (holyday day)
    where printDDate weekstr monthstr seasonstr yearstr holydaystr =
            "Today is " ++ weekstr ++ ", the " ++ monthstr ++ " day of " ++
            seasonstr ++ " in the YOLD " ++ yearstr ++ holydaystr

ddate :: IO String
ddate = do
        rand <- randomRIO (1, 10)::IO Integer
        if rand == 9
           then bollocks
           else righty
    where ddate' = liftM (date . toOrdinalDate . utctDay)
          bollocks = do
                time <- getCurrentTime
                ddate' $ return (addUTCTime 400000000 time)
          righty = ddate' getCurrentTime
