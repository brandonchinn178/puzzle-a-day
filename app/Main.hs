{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Monad (guard)
import Data.Maybe (fromMaybe)
import Data.Time (UTCTime (..), getCurrentTime, toGregorian)
import Options.Applicative
import Text.Read (readMaybe)
import Text.Printf (printf)

import SolverDFS qualified as DFS
import SolverGenetic qualified as Genetic
import Render (renderBoard, renderMonth)

data CLIOptions = CLIOptions
  { cliMonth :: Maybe Int
  , cliDay :: Maybe Int
  , cliAlgorithm :: Algorithm
  }

data Algorithm = DFS | Genetic

parseCLIOptions :: ParserInfo CLIOptions
parseCLIOptions = info (options <**> helper) fullDesc
  where
    options =
      CLIOptions
        <$> argument (Just <$> maybeReader parseMonth)
              ( metavar "MONTH"
              <> help "Month (1-12)"
              <> value Nothing
              )
        <*> argument (Just <$> maybeReader parseDay)
              ( metavar "DAY"
              <> help "Day (1-31)"
              <> value Nothing
              )
        <*> option (maybeReader parseAlgorithm)
              ( long "solver"
              <> help "choices: dfs (default), genetic"
              <> value DFS
              )

    parseMonth m = do
      m' <- readMaybe m
      guard (1 <= m' && m' <= 12)
      pure m'

    parseDay d = do
      d' <- readMaybe d
      guard (1 <= d' && d' <= 31)
      pure d'

    parseAlgorithm = \case
      "dfs" -> Just DFS
      "genetic" -> Just Genetic
      _ -> Nothing

main :: IO ()
main = do
  CLIOptions{..} <- execParser parseCLIOptions

  UTCTime today _ <- getCurrentTime
  let (_, currMonth, currDay) = toGregorian today

  let
    month = fromMaybe currMonth cliMonth
    day = fromMaybe currDay cliDay

  case cliAlgorithm of
    DFS -> do
      printf "\n============= Solving for %s %d =============\n\n" (renderMonth month) day
      let solution = DFS.solve (month, day)
      putStrLn (renderBoard solution)
    Genetic -> do
      let solution =
            Genetic.solve
              Genetic.SolveOptions
                { targetDate = (month, day)
                , logProgress = \generation board fitness -> do
                    clearScreen
                    printf "============= Solving for %s %d =============\n\n" (renderMonth month) day
                    printf "Generation: %d\n" generation
                    printf "Fitness: %d\n\n" fitness
                    putStrLn $ renderBoard board
                }
      solution `seq` putStrLn "Solved!"

clearScreen :: IO ()
clearScreen = putStr "\ESCc"
