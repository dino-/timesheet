{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Timesheet.Opts
  ( Options (..)
  , parseOpts
  )
  where

import Data.Version ( showVersion )
import Options.Applicative
import Paths_timesheet ( version )
import System.Environment ( getProgName )
import Text.Heredoc ( here )
import Text.PrettyPrint.ANSI.Leijen ( string )
import Text.Printf ( printf )


data Options = Options
  { optAll :: Bool
  , optWeeks :: Int
  , optTimesheetFile :: FilePath
  }


parser :: Parser Options
parser = Options
  <$> switch
    (  long "all"
    <> short 'a'
    <> help "Calculate totals for everything in the file"
    )
  <*> option auto
    (  long "weeks"
    <> short 'w'
    <> help "Calculate totals for the specified number of weeks, counting backwards from the top."
    <> showDefault
    <> value 1
    <> metavar "INT"
    )
  <*> argument str (metavar "FILE")


versionHelper :: String -> Parser (a -> a)
versionHelper progName =
  infoOption (progName ++ " " ++ showVersion version) $ mconcat
    [ long "version"
    , help "Show version information"
    , hidden
    ]


parseOpts :: IO Options
parseOpts = do
  pn <- getProgName
  execParser $ info (parser <**> helper <**> versionHelper pn)
    (  (header $ pn ++ " - Add up timesheet hours")
    <> footer'
    )


footer' :: InfoMod a
footer' = footerDoc . Just . string $ printf content (showVersion version)
    where content = init . tail $ [here|
Example data

Make a file looking like below. Days in each week in order, but the weeks
themselves in reverse order. The blank line following EACH WEEK is
important.

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

Version %s  Dino Morelli <dino@ui3.info>
|]
