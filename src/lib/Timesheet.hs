module Timesheet
  ( Day (..), Week
  , parse
  , parseLine
  )
  where

import Data.List ( groupBy )
import Data.Maybe ( catMaybes, isJust )
import Control.Arrow ( (&&&) )
import Safe ( readNote, tailNote )
import Text.Regex ( matchRegex, mkRegex, splitRegex )


data Day = Day String String Float
  deriving (Eq, Show)

type Week = [Day]


{- Group elements of a list into pairs:
      [1, 2, 3, 4, 5] -> [(1, 2), (3, 4)]
-}
listToPairs :: [a] -> [(a, a)]
listToPairs (x:y:zs) = (x, y) : listToPairs zs
listToPairs _        = []


-- Parse a time string in the form "hh:mm" into a Float hh.mmmm
parseTime :: String -> Float
parseTime "" = 0.0
parseTime s = h + (m / 60)
   where
      (h, m) = (readNote note . takeWhile (/= ':') &&&
         readNote note . tailNote note . dropWhile (/= ':')) s

      note = "Failed to parse: \"" ++ s ++ "\""


{- Parse a line from our data file format into a Day data
   Also computes the time values and sums them for the day.
-}
parseLine :: String -> Maybe Day
parseLine l = do
   [date, times] <- matchRegex (mkRegex "(.* [a-zA-Z]{2})[\t ]+(.*)") l
   let timeSum = sum . map (\(b, e) -> e - b) . listToPairs
         . map parseTime . splitRegex (mkRegex "[\t -]+") $ times
   return $ Day date times timeSum


parse :: String -> [Week]
parse hoursFileContents = allWeeks
  where
    -- Turn all lines into a list of Maybe Day, blanks are Nothing
    -- and serve to signify week divisions
    parsedLines = map parseLine . lines $ hoursFileContents

    -- Turn the list into a [[Day]] where each sublist is a week
    allWeeks = filter (not . null) . map catMaybes
        . groupBy (\x y -> isJust x && isJust y) $ parsedLines
