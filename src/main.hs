import Data.List ( groupBy )
import Data.Maybe ( catMaybes, isJust )
import Control.Arrow ( (&&&) )
import System.IO ( BufferMode ( NoBuffering )
   , hSetBuffering, stdout, stderr )
import Text.Printf ( printf )
import Text.Regex ( matchRegex, mkRegex, splitRegex )

import Timesheet.Opts
  ( Options(..), optTimesheetFile
  , parseOpts
  )


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


weeksToDisplay :: Options -> [Week] -> [Week]
weeksToDisplay opts allWs =
   case opts of
      Options True _     _ -> allWs
      Options _    numWs _ -> take numWs allWs


main :: IO ()
main = do
  -- No buffering, it messes with the order of output
  mapM_ (flip hSetBuffering NoBuffering) [stdout, stderr]

  opts <- parseOpts

  -- Turn all lines into a list of Maybe Day, blanks are Nothing
  -- and serve to signify week divisions
  parsedLines <- fmap (map parseLine . lines)
    $ readFile . optTimesheetFile $ opts

  -- Turn the list into a [[Day]] where each sublist is a week
  let weeks = filter (not . null) . map catMaybes
        . groupBy (\x y -> isJust x && isJust y) $ parsedLines

  mapM_ displayWeek $ weeksToDisplay opts weeks
