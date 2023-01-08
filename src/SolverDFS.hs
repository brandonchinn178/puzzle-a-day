{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module SolverDFS (
  solve,

  -- * Helpers
  BoardPieces (..),
  PieceAbsolute (..),
  Coordinate (..),
  isInBoard,
) where

import Control.Monad (guard)
import Data.Maybe (listToMaybe)
import Data.Set (Set)
import Data.Set qualified as Set

import PieceData (
  Pieces (..),
  PiecesField (..),
  PieceRelative (..),
  Vector (..),
  allPieceOrientations,
  piecesFields,
 )

{----- Solve -----}

-- | List of pieces, with each piece represented as a list of coordinates.
newtype BoardPieces = BoardPieces {unBoardPieces :: [PieceAbsolute]}
  deriving (Show)

emptyBoard :: BoardPieces
emptyBoard = BoardPieces []

-- | (Month 1-12, Day 1-31)
type Date = (Int, Int)

type PiecesSeen = Pieces Bool

solve :: Date -> BoardPieces
solve date =
  case (!! 8) $ iterate addNextPiece [(emptyBoard, emptyPiecesSeen)] of
    (board, _) : _ -> board
    [] -> error "Found no solution"
  where
    emptyPiecesSeen =
      Pieces
        { piece0 = False
        , piece1 = False
        , piece2 = False
        , piece3 = False
        , piece4 = False
        , piece5 = False
        , piece6 = False
        , piece7 = False
        }

    -- Add one more piece to every board
    addNextPiece :: [(BoardPieces, PiecesSeen)] -> [(BoardPieces, PiecesSeen)]
    addNextPiece states =
      flip concatMap states $ \(board, seenPieces) ->
        flip concatMap piecesFields $ \PiecesField{..} ->
          let
            seenPiece = getPiece seenPieces
            pieceOrientations = getPiece allPieceOrientations
          in if seenPiece
              then []
              else
                [ (board', setPiece True seenPieces)
                | Just board' <- map (tryAddPiece date board) pieceOrientations
                ]

-- | Try adding the given oriented piece to the given board
tryAddPiece :: Date -> BoardPieces -> PieceRelative -> Maybe BoardPieces
tryAddPiece (month, day) board piece = do
  coord <- firstEmptySpot effectiveBoard
  let piece' = toPieceAbsolute coord piece
  -- ensure piece is on the board
  guard $ all isInBoard . unPieceAbsolute $ piece'
  -- ensure piece is not overlapping other pieces or the date
  guard $ Set.disjoint (unPieceAbsolute piece') (allBoardCoordinates effectiveBoard)
  pure $ unsafeAddPiece piece' board
  where
    unsafeAddPiece p = BoardPieces . (p :) . unBoardPieces

    -- board with the date spots filled in
    effectiveBoard = unsafeAddPiece datePiece board
    datePiece =
      PieceAbsolute . Set.fromList $
        [ monthToCoordinate month
        , dayToCoordinate day
        ]

firstEmptySpot :: BoardPieces -> Maybe Coordinate
firstEmptySpot board =
  listToMaybe
    [ coord
    | x <- [0..6]
    , y <- [0..6]
    , let coord = Coordinate (x, y)
    , isInBoard coord
    , coord `Set.notMember` allBoardCoordinates board
    ]

allBoardCoordinates :: BoardPieces -> Set Coordinate
allBoardCoordinates = Set.unions . map unPieceAbsolute . unBoardPieces

{----- Absolute pieces -----}

-- | A piece represented as the list of coordinates it covers.
newtype PieceAbsolute = PieceAbsolute {unPieceAbsolute :: Set Coordinate}
  deriving (Show, Eq, Ord)

toPieceAbsolute :: Coordinate -> PieceRelative -> PieceAbsolute
toPieceAbsolute (Coordinate (x, y)) (PieceRelative pieceVectors) =
  PieceAbsolute $ Set.map fromVector pieceVectors
  where
    fromVector (Vector (dx, dy)) = Coordinate (x + dx, y + dy)

newtype Coordinate = Coordinate (Int, Int)
  deriving (Show, Eq, Ord)

isInBoard :: Coordinate -> Bool
isInBoard (Coordinate (x, y)) =
  or
    [ (0 <= y && y <= 1) && (0 <= x && x <= 5)
    , (2 <= y && y <= 5) && (0 <= x && x <= 6)
    , y == 6 && (0 <= x && x <= 2)
    ]

monthToCoordinate :: Int -> Coordinate
monthToCoordinate month =
  let (y, x) = (month - 1) `divMod` 6
   in Coordinate (x, y)

dayToCoordinate :: Int -> Coordinate
dayToCoordinate day =
  let (y, x) = (day - 1) `divMod` 7
   in Coordinate (x, y + 2)
