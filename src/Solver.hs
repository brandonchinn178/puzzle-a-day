{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}

module Solver (
  solve,

  -- * Types
  CompletedBoard (..),
  Coordinate,
  Col (..),
  Row (..),
) where

{----- Coordinate -----}

type Coordinate = (Col, Row)

-- | Corresponds to the x-coordinate. Guaranteed to be 0 to 6
newtype Col = Col {unCol :: Int}
  deriving (Show, Eq, Ord)

-- | Corresponds to the y-coordinate. Guaranteed to be 0 to 6
newtype Row = Row {unRow :: Int}
  deriving (Show, Eq, Ord)

{----- Solve -----}

type Month = Int
type Day = Int

-- | List of pieces, with each piece represented as a list of coordinates.
data CompletedBoard = CompletedBoard [[Coordinate]]

solve :: (Month, Day) -> CompletedBoard
solve _ = CompletedBoard
  [ [(Col 0, Row 0), (Col 0, Row 1), (Col 1, Row 1), (Col 0, Row 2)]
  , [(Col 3, Row 4)]
  ]
