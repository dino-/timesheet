import Test.Tasty
import Test.Tasty.HUnit
import Timesheet ( Day (..), parseLine )


main :: IO ()
main = defaultMain tests


tests :: TestTree
tests = testGroup " tests" [parseLineTests]


parseLineTests :: TestTree
parseLineTests = testGroup "parseLine tests"
  [ testCase "One time interval" $
      parseLine "2011-03-12 Sa  10:00-14:00"
        @?= Just (Day "2011-03-12 Sa" "10:00-14:00" 4.0)

  , testCase "Several time intervals" $
      parseLine "2011-03-15 Tu  09:00-11:30  12:15-17:15  18:30-22:00"
        @?= Just (Day "2011-03-15 Tu" "09:00-11:30  12:15-17:15  18:30-22:00" 11.0)

  , testCase "Several time intervals, spaces instead of hyphens" $
      parseLine "2011-03-15 Tu  09:00 11:30  12:15  17:15  18:30 22:00"
        @?= Just (Day "2011-03-15 Tu" "09:00 11:30  12:15  17:15  18:30 22:00" 11.0)

  , testCase "A blank line" $
      parseLine "" @?= Nothing

  , testCase "Some garbage input" $
      parseLine "foo bar baz 27851 23:33" @?= Nothing
  ]
