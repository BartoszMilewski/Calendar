module Main where

import Data.List
import Data.List.Split (chunk)

import Data.Time
import Data.Time.Calendar.WeekDate (toWeekDate)
import System.Locale (defaultTimeLocale)

month :: Day -> Int
month d = let (_, m, _) = toGregorian d
          in m

week_number :: Day -> Int
week_number d = let (_, w, _) = toWeekDate d
                in w

dayOfWeek :: Day -> Int
dayOfWeek d = let (_, _, dw) = toWeekDate (addDays 1 d)
              in dw - 1

byMonth :: [Day] -> [[Day]]
byMonth = groupBy sameMonth
  where
    sameMonth d1 d2 = month d1 == month d2

byWeek :: [Day] -> [[Day]]
byWeek = groupBy sameWeek
  where -- carve weeks from Sun to Sat rather than Mon to Sun
      sameWeek d1 d2 = week_number (addDays 1 d1) == week_number (addDays 1 d2)

byWeekMonth :: [Day] -> [[[Day]]]
byWeekMonth dates = fmap byWeek $ byMonth dates

-- Convert each day to a string, padded day of the month, paired with the date
formatDays :: [[[Day]]] -> [[[(String, Day)]]]
formatDays =(fmap.fmap.fmap) (\d -> (formatTime defaultTimeLocale "%_d" d, d))

-- Convert each week to string of days of month
-- First week of the month padded on the left
-- Last week padded on the right
-- paired with the starting date of the week
formatWeeks :: [[[(String, Day)]]] -> [[(String, Day)]]
formatWeeks = (fmap.fmap) (\sds -> formatW $ unzip sds)
  where
    padL d = replicate (3 * dayOfWeek d) ' '
    padR d = replicate (3 * (6 - dayOfWeek d)) ' '
    formatW :: ([String], [Day]) -> (String, Day)
    formatW (ss, ds) =  (padL (head ds) ++ intercalate " " ss ++ padR (last ds), head ds)

-- Concatenate weeks to months, 
-- paired with the starting date of the month
formatMonths :: [[(String, Day)]] -> [(String, Day)]
formatMonths = fmap (\sds -> formatM $ unzip sds)
  where
    formatM :: ([String], [Day]) -> (String, Day)
    formatM (ss, ds) = 
        let wks = length ss
            ss' = ss ++ replicate (6 - wks) (replicate 20 ' ')
        in
           (intercalate "\n" ss', head ds)

-- Prepend each month with its name, drop the dates
titleMonths :: [(String, Day)] -> [String]
titleMonths sds =  fmap (\(s, d) -> padMonth d ++ "\n" ++ s) sds
  where
        padMonth d = let s = monthName d
                         len = length s
                         padL = (21 - len) `div` 2
                         padR = 20 - padL - len
                     in replicate padL ' ' ++ s ++ replicate padR ' '
        monthName d = formatTime defaultTimeLocale "%B" d

transposeMonths :: [[String]] -> [[String]]
transposeMonths = fmap transposeChunk
  where
    transposeChunk ch = fmap (intercalate "   ") $ transpose $ fmap lines ch

-- works with infinite range
formatCalendar :: Integer -> String
formatCalendar year = 
         intercalate "\n" $
         fmap (intercalate "\n") $
         transposeMonths $
         chunk 3 $
         take 12 $
         titleMonths $
         formatMonths $
         formatWeeks $
         formatDays $
         byWeekMonth [fromGregorian year 1 1..]

main :: IO ()
main = putStr $ formatCalendar 2015
