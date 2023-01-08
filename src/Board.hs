module Board (
  -- * BoardPieces
  BoardPieces (..),
  emptyBoard,
  firstEmptySpot,
  allBoardCoordinates,

  -- * PieceAbsolute
  PieceAbsolute (..),
  toPieceAbsolute,

  -- * Coordinate
  Coordinate (..),
  isInBoard,
  monthToCoordinate,
  dayToCoordinate,
) where

import Data.Maybe (listToMaybe)
import Data.Set (Set)
import Data.Set qualified as Set

import PieceData (PieceRelative (..), Vector (..))

{----- BoardPieces -----}

-- | List of pieces, with each piece represented as a list of coordinates.
newtype BoardPieces = BoardPieces {unBoardPieces :: [PieceAbsolute]}
  deriving (Show)

emptyBoard :: BoardPieces
emptyBoard = BoardPieces []

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

{----- Coordinate -----}

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
