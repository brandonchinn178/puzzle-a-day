{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Solver (
  solve,
) where

import Control.Monad (guard)
import Data.Set qualified as Set

import Board (
  BoardPieces (..),
  PieceAbsolute (..),
  allBoardCoordinates,
  dayToCoordinate,
  emptyBoard,
  firstEmptySpot,
  isInBoard,
  monthToCoordinate,
  toPieceAbsolute,
 )
import PieceData (
  Pieces (..),
  PiecesField (..),
  PieceRelative (..),
  allPieceOrientations,
  piecesFields,
 )

-- | (Month 1-12, Day 1-31)
type Date = (Int, Int)

type PiecesSeen = Pieces Bool

solve :: Date -> Maybe BoardPieces
solve date =
  case (!! 8) $ iterate (concatMap addNextPiece) [(emptyBoard, emptyPiecesSeen)] of
    (board, _) : _ -> Just board
    [] -> Nothing
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

    -- Add one more piece to the board
    addNextPiece :: (BoardPieces, PiecesSeen) -> [(BoardPieces, PiecesSeen)]
    addNextPiece (board, seenPieces) =
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
