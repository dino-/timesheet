import System.IO ( BufferMode ( NoBuffering )
   , hSetBuffering, stdout, stderr )
import Text.Printf ( printf )
import Timesheet ( Day (..), Week, parse )

import Opts
  ( Options (Options), optTimesheetFile
  , parseOpts
  )


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
  hoursFileContents <- readFile . optTimesheetFile $ opts

  mapM_ displayWeek $ weeksToDisplay opts . parse $ hoursFileContents
