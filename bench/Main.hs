import Criterion.Main
import Text.Printf (printf)

import Render (renderMonth)
import Solver (solve)

main :: IO ()
main = defaultMain $ benchAllDates (nf show . solve)
  where
    benchAllDates f =
      [ bench (printf "%s %d" (renderMonth month) day) (f (month, day))
      | month <- [1..12]
      , day <- [1..31]
      ]
