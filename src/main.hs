-- Copyright: 2011 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

import Data.List
import Data.Maybe
import Control.Arrow
import System.Environment
import Text.Printf
import Text.Regex


data Day = Day String String Float

type Week = [Day]


{- Group elements of a list into pairs:
      [1, 2, 3, 4, 5] -> [(1, 2), (3, 4)]
-}
listToPairs :: [a] -> [(a, a)]
listToPairs (x:y:zs) = (x, y) : listToPairs zs
listToPairs _        = []


-- Parse a time string in the form "hh:mm" into a Float hh.mmmm
parseTime :: String -> Float
parseTime s = h + (m / 60)
   where
      (h, m) = (read . takeWhile (/= ':') &&& 
         read . tail . dropWhile (/= ':')) s


{- Parse a line from our data file format into a Day data
   Also computes the time values and sums them for the day.
-}
parseLine :: String -> Maybe Day
parseLine l = do
   [date, times] <- matchRegex (mkRegex "(.* [a-zA-Z]{2})[\t ]+(.*)") l
   let timeSum = sum . map (\(b, e) -> e - b) . listToPairs
         . map parseTime . splitRegex (mkRegex "[\t -]+") $ times
   return $ Day date times timeSum


{- Take a list of Day representing a single week and display it
   Computes the total week hours and displays that as well.
-}
displayWeek :: Week -> IO ()
displayWeek w = do
   let weekTotal = sum . map (\(Day _ _ h) -> h) $ w

   mapM_ (\(Day d t h) -> printf "%s  %5.2f  %s\n" d h t) w
   printf "total:         %5.2f\n\n" weekTotal


main :: IO ()
main = do
   (path:_) <- getArgs

   -- Turn all lines into a list of Maybe Day, blanks are Nothing
   -- and serve to signify week divisions
   parsedLines <- fmap (map parseLine . lines) $ readFile path

   -- Turn the list into a [[Day]] where each sublist is a week
   let weeks = filter (not . null) . map catMaybes
         . groupBy (\x y -> isJust x && isJust y) $ parsedLines

   mapM_ displayWeek . take 1 $ weeks


{-

Example data. Make a file looking like below. Days in each week in
order, but the weeks themselves in reverse order. The blank line
following EACH WEEK is important.

Days can be skipped (like weekends)
There can be one or more time ranges within each day as shown

-----
2011-03-12 Sa  10:00-14:00
2011-03-13 Su  10:00-13:00
2011-03-14 Mo  09:00-11:00
2011-03-15 Tu  09:00-11:30  12:15-17:15  18:30-22:00
2011-03-16 We  09:00-12:00  13:00-13:30

2011-03-08 Tu  13:30-17:30
2011-03-09 We  09:00-12:00  13:30-19:00
2011-03-10 Th  08:30-11:45  13:30-16:30
2011-03-11 Fr  09:00-10:15

-----
-}
