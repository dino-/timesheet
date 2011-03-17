-- Copyright: 2011 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>


module Timesheet.Opts
   ( Options (..)
   , parseOpts, usageText
   )
   where

import Data.Maybe
import System.Console.GetOpt
import System.Exit


data Options = Options
   { optAll :: Bool
   , optHelp :: Bool
   , optWeeks :: Int
   }


defaultOptions :: Options
defaultOptions = Options
   { optAll = False
   , optHelp = False
   , optWeeks = 1
   }


options :: [OptDescr (Options -> Options)]
options =
   [ Option ['a'] ["all"] 
      (NoArg (\opts -> opts { optAll = True } ))
      "Calculate totals for everything in the file"
   , Option ['h'] ["help"] 
      (NoArg (\opts -> opts { optHelp = True } ))
      "This help text"
   , Option ['w'] ["weeks"]
      ( OptArg
         ((\n opts -> opts { optWeeks = read n}) . fromMaybe "1")
         "INT")
      "Calculate totals for the specified number of weeks, counting backwards from the top. Default: 1"
   ]


parseOpts :: [String] -> IO (Either ExitCode (Options, [String]))
parseOpts argv = 
   case getOpt Permute options argv of
      (o,n,[]  ) -> return . Right $ (foldl (flip id) defaultOptions o, n)
      (_,_,errs) -> do
         putStrLn $ concat errs ++ usageText
         return . Left $ ExitFailure 1


usageText :: String
usageText = (usageInfo header options) ++ "\n" ++ footer
   where
      header = init $ unlines
         [ "Usage: timesheet [OPTIONS] FILE"
         , "Add up timesheet hours"
         , ""
         , "Options:"
         ]
      footer = init $ unlines
         [ "Example data"
         , ""
         , "Make a file looking like below. Days in each week in order, but the weeks"
         , "themselves in reverse order. The blank line following EACH WEEK is"
         , "important."
         , ""
         , "Days can be skipped (like weekends)"
         , "There can be one or more time ranges within each day as shown"
         , ""
         , "-----"
         , "2011-03-12 Sa  10:00-14:00"
         , "2011-03-13 Su  10:00-13:00"
         , "2011-03-14 Mo  09:00-11:00"
         , "2011-03-15 Tu  09:00-11:30  12:15-17:15  18:30-22:00"
         , "2011-03-16 We  09:00-12:00  13:00-13:30"
         , ""
         , "2011-03-08 Tu  13:30-17:30"
         , "2011-03-09 We  09:00-12:00  13:30-19:00"
         , "2011-03-10 Th  08:30-11:45  13:30-16:30"
         , "2011-03-11 Fr  09:00-10:15"
         , ""
         , "-----"
         , ""
         , "Version 1.0.0.0  Dino Morelli <dino@ui3.info>"
         ]
