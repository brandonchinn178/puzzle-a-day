{-# LANGUAGE LambdaCase #-}

import Data.Time (UTCTime (..), getCurrentTime, toGregorian)
import System.Environment (getArgs)
import Text.Read (readMaybe)
import Text.Printf (printf)

import Solver (solve)
import Render (renderBoard, renderMonth)

main :: IO ()
main = do
  (month, day) <-
    getArgs >>= \case
      [] -> do
        UTCTime today _ <- getCurrentTime
        let (_, m, d) = toGregorian today
        pure (m, d)
      [m, d]
        | Just m' <- readMaybe m
        , Just d' <- readMaybe d
        , 1 <= m', m' <= 12
        , 1 <= d', d' <= 31 ->
            pure (m', d')
      _ -> error $ "Usage: ./puzzle-a-day [MONTH DAY]"

  printf "\n============= Solving for %s %d =============\n\n" (renderMonth month) day

  let solution = solve (month, day)
  putStrLn (renderBoard solution)
