{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module SolverGenetic (
  solve,
  SolveOptions (..),
) where

import Control.Arrow ((&&&))
import Control.Monad (replicateM)
import Control.Monad.Random.Strict (
  MonadRandom,
  Rand,
  StdGen,
  evalRand,
  getRandom,
  getRandomR,
  initStdGen,
  interleave,
  weighted,
 )
import Control.Parallel.Strategies (parTuple2, parMap, r0, rpar)
import Data.List (sortOn)
import Data.Maybe (catMaybes)
import Data.MultiSet qualified as MultiSet
import Data.Ord (Down (..))
import System.IO.Unsafe (unsafePerformIO)

import Board (
  BoardPieces (..),
  Coordinate (..),
  PieceAbsolute (..),
  dayToCoordinate,
  emptyBoard,
  isInBoard,
  monthToCoordinate,
  toPieceAbsolute,
 )
import PieceData (
  Pieces (..),
  PieceRelative (..),
  allPieceOrientations,
  fromPieces,
  zipWithPieces,
  zipWithPiecesM,
 )

-- | (Month 1-12, Day 1-31)
type Date = (Int, Int)

data SolveOptions = SolveOptions
  { targetDate :: Date
  , logProgress :: Integer -> BoardPieces -> Fitness -> IO ()
  }

type SolveM = Rand StdGen
type Chromosome = BoardState

solve :: SolveOptions -> BoardPieces
solve SolveOptions{..} =
  (`evalRand` gen) $ do
    population <- replicateM populationSize genBoardState
    logProgress' 0 emptyBoard 0
    go 0 population
  where
    gen = unsafePerformIO initStdGen
    logProgress' i board fitness = unsafePerformIO $ logProgress i board fitness >> pure (pure ())

    go i population = do
      let
        populationWithFitness = parMap (parTuple2 r0 rpar) (id &&& getFitness targetDate) population
        sortedPopulation = sortOn (Down . snd) populationWithFitness
        (fittestChild, fittestScore) = head sortedPopulation
        bestBoard = toBoard fittestChild

      logProgress' (i + 1) bestBoard fittestScore

      if fittestScore == maxFitness
        then pure bestBoard
        else do
          population' <- genNewGeneration sortedPopulation
          go (i + 1) population'

genNewGeneration :: [(Chromosome, Fitness)] -> SolveM [Chromosome]
genNewGeneration sortedPopulation = do
  -- Choose elite children
  let eliteChildren = take numEliteChildren . map fst $ sortedPopulation

  -- Select 2 different parents randomly, weighted by fitness
  (parent1, parent2) <- loopM $ do
    let selectParent = weighted' $ map (fmap fromIntegral) sortedPopulation
    parent1 <- selectParent
    parent2 <- selectParent
    pure $
      if parent1 == parent2
        then Nothing
        else Just (parent1, parent2)

  -- Create new children from parents
  crossoverChildren <- replicateM numCrossoverChildren (interleave $ doCrossover parent1 parent2)

  pure $ eliteChildren ++ crossoverChildren
  where
    loopM f = f >>= maybe (loopM f) pure

    -- shift so total weight > 0
    weighted' xs =
      let minWeight = minimum (map snd xs)
       in weighted $ map (\(a, weight) -> (a, minWeight + weight)) xs

doCrossover :: Chromosome -> Chromosome -> SolveM Chromosome
doCrossover (BoardState parent1) (BoardState parent2) =
  zipWithPiecesM crossover parent1 parent2
    >>= traverse mutate
    >>= pure . BoardState
  where
    crossover pieceState1 pieceState2 =
      PieceState
        <$> pick (isPlayed pieceState1) (isPlayed pieceState2)
        <*> pick (coordinate pieceState1) (coordinate pieceState2)
        <*> pick (orientation pieceState1) (orientation pieceState2)

    mutate pieceState =
      PieceState
        <$> pick' mutationRate getRandom (pure $ isPlayed pieceState)
        <*> pick' mutationRate genCoordinate (pure $ coordinate pieceState)
        <*> pick' mutationRate genOrientation (pure $ orientation pieceState)

    pick a b = pick' 0.5 (pure a) (pure b)

    pick' cutoff a b = do
      x <- getRandom
      if x < (cutoff :: Double) then a else b

{----- Parameters -----}

populationSize :: Int
populationSize = 1000

numEliteChildren :: Int
numEliteChildren = round (0.1 * fromIntegral populationSize :: Rational)

numCrossoverChildren :: Int
numCrossoverChildren = populationSize - numEliteChildren

mutationRate :: Double
mutationRate = 0.1

{----- SolveState -----}

newtype BoardState = BoardState (Pieces PieceState)
  deriving (Show, Eq)

data PieceState = PieceState
  { isPlayed :: Bool
  , coordinate :: Coordinate
  , orientation :: Int
  }
  deriving (Show, Eq)

genBoardState :: MonadRandom m => m BoardState
genBoardState =
  fmap BoardState $
    Pieces
      <$> genPieceState
      <*> genPieceState
      <*> genPieceState
      <*> genPieceState
      <*> genPieceState
      <*> genPieceState
      <*> genPieceState
      <*> genPieceState

genPieceState :: MonadRandom m => m PieceState
genPieceState =
  PieceState
    <$> getRandom
    <*> genCoordinate
    <*> genOrientation

genOrientation :: MonadRandom m => m Int
genOrientation = getRandomR (0, 10)

genCoordinate :: MonadRandom m => m Coordinate
genCoordinate = do
  x <- getRandomR (0, 6)
  y <- getRandomR (0, 6)
  pure $ Coordinate (x, y)

toBoard :: BoardState -> BoardPieces
toBoard (BoardState pieceStates) =
  BoardPieces $
    catMaybes . fromPieces $
      zipWithPieces resolvePiece pieceStates allPieceOrientations
  where
    resolvePiece :: PieceState -> [PieceRelative] -> Maybe PieceAbsolute
    resolvePiece PieceState{..} pieceOrientations =
      if isPlayed
        then Just $ toPieceAbsolute coordinate $ pieceOrientations `get` orientation
        else Nothing

    -- same as !!, except wraps overflow or negative indexes
    list `get` i =
      let i' = i `mod` length list
       in list !! i'

{----- Fitness -----}

type Fitness = Int

-- | The max fitness, where all squares except the target date are covered
maxFitness :: Fitness
maxFitness = 41

getFitness :: Date -> BoardState -> Fitness
getFitness (month, day) boardState =
  numSquaresCovered
    - 5 * dateSquaresCovered
    - 2 * overlappingSquares
    - 2 * numSquaresOffBoard
  where
    BoardPieces pieces = toBoard boardState
    coordinates = MultiSet.unions $ map (\(PieceAbsolute cs) -> MultiSet.fromSet cs) pieces

    numSquaresCovered = MultiSet.distinctSize coordinates
    dateSquaresCovered =
      sum
        [ if monthToCoordinate month `MultiSet.member` coordinates then 1 else 0
        , if dayToCoordinate day `MultiSet.member` coordinates then 1 else 0
        ]
    overlappingSquares = countWhere ((> 1) . snd) $ MultiSet.toOccurList coordinates
    numSquaresOffBoard = countWhere (not . isInBoard) $ MultiSet.distinctElems coordinates

    countWhere f = length . filter f
