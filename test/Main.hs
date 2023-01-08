import Data.Maybe (isJust)
import Test.Tasty
import Test.Tasty.HUnit
import Text.Printf (printf)

import Render (renderMonth)
import Solver (solve)

main :: IO ()
main = defaultMain . testGroup "puzzle-a-day" $
  testAllDates (assertBool "no solution found" . isJust . solve)
  where
    testAllDates f =
      [ testCase (printf "%s %d" (renderMonth month) day) (f (month, day))
      | month <- [1..12]
      , day <- [1..31]
      ]
