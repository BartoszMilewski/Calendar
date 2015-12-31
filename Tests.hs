module Tests where

import Data.List
import Data.List.Split

import Data.Time
import Data.Time.Calendar.WeekDate
import System.Locale

test1 = print [1..10]

from = fromGregorian 2015 1 1
to   = fromGregorian 2015 12 31

test2 = print [from .. to]

test3 = putStr $ concat $ take 10 $ fmap (formatTime defaultTimeLocale "%Y-%b-%d%n") [from .. to]

month d = let (_, m, _) = toGregorian d
          in m

sameMonth d1 d2 = month d1 == month d2

test4 = print $ fmap head $ groupBy sameMonth [from..to]

byMonth :: [Day] -> [[Day]]
byMonth = groupBy sameMonth

test5 = print $ fmap head $ byMonth [from..to]

week_number d = let (_, w, _) = toWeekDate d
                in w
dayOfWeek d = let (_, _, dw) = toWeekDate (addDays 1 d)
              in dw - 1

byWeek :: [Day] -> [[Day]]
byWeek = groupBy sameWeek
  where -- carve weeks from Sun to Sat rather than Mon to Sun
      sameWeek d1 d2 = week_number (addDays 1 d1) == week_number (addDays 1 d2)

test6 = print $ fmap byWeek $ byMonth [from..to]

byWeekMonth :: [Day] -> [[[Day]]]
byWeekMonth dates = fmap byWeek $ byMonth dates

toMofWofD :: [Day] -> [[[String]]]
toMofWofD = (fmap . fmap . fmap) (formatTime defaultTimeLocale "%_d") . byWeekMonth

test7 = putStr $ intercalate "\n----\n" $ fmap (intercalate "\n") $ (fmap.fmap) (intercalate " ") $ toMofWofD [from..to]

-- Serious formatting

datesInYear y =  [from .. to]
  where
    from = fromGregorian y 1 1
    to   = fromGregorian y 12 31

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

test8 = putStr $ intercalate "\n----\n" $ fmap fst $ formatMonths $ formatWeeks $ formatDays $ byWeekMonth (datesInYear 2015)

-- Prepend each month with its name, drop all dates
titleMonths :: [(String, Day)] -> [String]
titleMonths sds =  fmap (\(s, d) -> padMonth d ++ "\n" ++ s) sds
  where
      padMonth d = let s = monthName d
                       len = length s
                       padL = (21 - len) `div` 2
                       padR = 20 - padL - len
                   in replicate padL ' ' ++ s ++ replicate padR ' '


monthName :: Day -> String
monthName d = s
  where
    s = formatTime defaultTimeLocale "%B" d

test9 = putStr $ intercalate "\n\n" $ titleMonths $ formatMonths $ formatWeeks $ formatDays $ byWeekMonth (datesInYear 2015)
test10 = putStr $ intercalate "\n\n" $ take 24 $ titleMonths $ formatMonths $ formatWeeks $ formatDays $ byWeekMonth [fromGregorian 2015 1 1..]
test11 = print $ chunk 3 [0..9]

test12 = putStr  $ 
         intercalate "\n\n" $ 
         fmap (intercalate "\n") $ 
         transposeMonths $ 
         chunk 3 $ 
         titleMonths $ 
         formatMonths $ 
         formatWeeks $ 
         formatDays $ 
         byWeekMonth (datesInYear 2015)

-- works with infinite range
test13 = putStr  $ 
         intercalate "\n" $
         fmap (intercalate "\n") $ 
         transposeMonths $ 
         chunk 3 $
         take 13 $
         titleMonths $ 
         formatMonths $ 
         formatWeeks $ 
         formatDays $ 
         byWeekMonth [fromGregorian 2015 1 1..]

-- in: list of chunks of three months
transposeMonths :: [[String]] -> [[String]]
transposeMonths = fmap transposeChunk
  where
    transposeChunk :: [String] -> [String]
    transposeChunk ch = fmap (intercalate "   ") $ transpose $ fmap lines ch

