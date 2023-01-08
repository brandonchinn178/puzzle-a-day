module Render (
  renderBoard,
  renderMonth,
) where

import Control.Monad (forM_, when)
import Control.Monad.ST (ST, runST)
import Data.Array (Array, (!))
import Data.Array.ST (STArray, freeze, newArray, readArray, writeArray)

import SolverDFS (
  BoardPieces (..),
  Coordinate (..),
  PieceAbsolute (..),
  isInBoard,
 )

renderBoard :: BoardPieces -> String
renderBoard board =
  unlines $
    topBorder : concat
      [ [ renderUpperCells y
        , renderLowerCells y
        , renderBottomCells y
        ]
      | y <- [0..6]
      ]
  where
    boardInfo = toBoardInfo board

    topBorder =
      concat
        [ start ++ concatRep 5 "─" ++ "┼"
        | x <- [0..5]
        , let start = if x == (0 :: Int) then "┼" else ""
        ]

    renderCellsWith f y =
      concat
        [ start ++ f (x, y) ++ end
        | x <- [0..6]
        , isInBoard' (x, y)
        , let
            cellInfo = boardInfo ! (x, y)
            start = if x == 0 then "│" else ""
            end = if hasRightBorder cellInfo then "│" else " "
        ]

    renderUpperCells =
      renderCellsWith $ \(x, y) ->
        if isVisible (boardInfo ! (x, y))
          then " " ++ renderCell (x, y) ++ " "
          else concatRep 5 " "

    renderLowerCells = renderCellsWith $ \_ -> concatRep 5 " "

    renderBottomCells y =
      concat
        [ start ++ concatRep 5 (if hasBottomBorder cellInfo then "─" else " ") ++ "┼"
        | x <- [0..6]
        , isInBoard' (x, y) || isInBoard' (x, y + 1)
        , let
            cellInfo = boardInfo ! (x, y)
            start = if x == 0 then "┼" else ""
        ]

-- | Render the three-character string in the given cell.
--
-- Assumes the coordinate is in the board.
renderCell :: (Int, Int) -> String
renderCell (x, y) =
  if y == 0 || y == 1
    then renderMonth (6 * y + x + 1)
    else renderDay (7 * (y - 2) + x + 1)
  where
    renderDay d =
      let s = show d
       in s ++ replicate (3 - length s) ' '

type BoardInfo array = array (Int, Int) CellInfo

data CellInfo = CellInfo
  { hasBottomBorder :: Bool
  , hasRightBorder :: Bool
  , isVisible :: Bool
  }

toBoardInfo :: BoardPieces -> BoardInfo Array
toBoardInfo (BoardPieces pieces) =
  runST $ do
    boardInfo <- newArray ((0, 0), (6, 6)) initialCellInfo
    mapM_ (updateBoardInfo boardInfo) pieces
    freeze boardInfo
  where
    initialCellInfo =
      CellInfo
        { hasBottomBorder = True
        , hasRightBorder = True
        , isVisible = True
        }

    updateBoardInfo :: BoardInfo (STArray s) -> PieceAbsolute -> ST s ()
    updateBoardInfo boardInfo (PieceAbsolute pieceCoordinates) = do
      forM_ pieceCoordinates $ \(Coordinate (x, y)) ->
        when (isInBoard' (x, y)) $ do
          -- mark coordinate as not visible
          modifyArray boardInfo (x, y) $ \cellInfo -> cellInfo{isVisible = False}

          -- update borders
          forM_ pieceCoordinates $ \(Coordinate (x', y')) -> do
            when (x' - x == 1 && y' - y == 0) $
              modifyArray boardInfo (x, y) $ \cellInfo -> cellInfo{hasRightBorder = False}
            when (y' - y == 1 && x' - x == 0) $
              modifyArray boardInfo (x, y) $ \cellInfo -> cellInfo{hasBottomBorder = False}

    modifyArray boardInfo coord f = do
      cellInfo <- readArray boardInfo coord
      writeArray boardInfo coord (f cellInfo)

isInBoard' :: (Int, Int) -> Bool
isInBoard' = isInBoard . Coordinate

{----- Utilities -----}

concatRep :: Int -> [a] -> [a]
concatRep n xs = concat $ replicate n xs

-- | Render month, with 1 = JAN, ...
renderMonth :: Int -> String
renderMonth n = months !! (n - 1)
  where
    months =
      [ "JAN"
      , "FEB"
      , "MAR"
      , "APR"
      , "MAY"
      , "JUN"
      , "JUL"
      , "AUG"
      , "SEP"
      , "OCT"
      , "NOV"
      , "DEC"
      ]
