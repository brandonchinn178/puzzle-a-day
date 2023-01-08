{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module PieceData (
  Pieces (..),
  zipWithPieces,
  zipWithPiecesM,
  PiecesField (..),
  piecesFields,
  fromPieces,
  PieceRelative (..),
  Vector (..),
  allPieceOrientations,
) where

import Data.Containers.ListUtils (nubOrd)
import Data.Functor.Identity (runIdentity)
import Data.Set (Set)
import Data.Set qualified as Set

data Pieces a = Pieces
  { piece0 :: a
  , piece1 :: a
  , piece2 :: a
  , piece3 :: a
  , piece4 :: a
  , piece5 :: a
  , piece6 :: a
  , piece7 :: a
  }
  deriving (Show, Eq, Functor, Foldable, Traversable)

zipWithPieces :: (a -> b -> c) -> Pieces a -> Pieces b -> Pieces c
zipWithPieces f as bs = runIdentity $ zipWithPiecesM (\a b -> pure (f a b)) as bs

zipWithPiecesM :: Applicative f => (a -> b -> f c) -> Pieces a -> Pieces b -> f (Pieces c)
zipWithPiecesM f as bs =
  Pieces
    <$> f (piece0 as) (piece0 bs)
    <*> f (piece1 as) (piece1 bs)
    <*> f (piece2 as) (piece2 bs)
    <*> f (piece3 as) (piece3 bs)
    <*> f (piece4 as) (piece4 bs)
    <*> f (piece5 as) (piece5 bs)
    <*> f (piece6 as) (piece6 bs)
    <*> f (piece7 as) (piece7 bs)

data PiecesField = PiecesField
  { getPiece :: forall a. Pieces a -> a
  , setPiece :: forall a. a -> Pieces a -> Pieces a
  }

piecesFields :: [PiecesField]
piecesFields =
  [ PiecesField piece0 (\a pieces -> pieces{piece0 = a})
  , PiecesField piece1 (\a pieces -> pieces{piece1 = a})
  , PiecesField piece2 (\a pieces -> pieces{piece2 = a})
  , PiecesField piece3 (\a pieces -> pieces{piece3 = a})
  , PiecesField piece4 (\a pieces -> pieces{piece4 = a})
  , PiecesField piece5 (\a pieces -> pieces{piece5 = a})
  , PiecesField piece6 (\a pieces -> pieces{piece6 = a})
  , PiecesField piece7 (\a pieces -> pieces{piece7 = a})
  ]

fromPieces :: Pieces a -> [a]
fromPieces pieces = map (($ pieces) . getPiece) piecesFields

-- | A piece represented as a list of vectors relative to some starting point.
newtype PieceRelative = PieceRelative {unPieceRelative :: Set Vector}
  deriving (Show, Eq, Ord)

-- | A tuple (dx, dy) indicating a change in coordinates.
newtype Vector = Vector (Int, Int)
  deriving (Show, Eq, Ord)

-- | All the pieces in the puzzle.
allPieces :: Pieces PieceRelative
allPieces =
  Pieces
    { piece0 = mkPiece [(0, 0), (0, 1), (1, 1), (2, 1), (2, 0)]
    , piece1 = mkPiece [(0, 0), (1, 0), (1, 1), (2, 0), (3, 0)]
    , piece2 = mkPiece [(0, 0), (0, 1), (1, 0), (1, 1), (2, 0), (2, 1)]
    , piece3 = mkPiece [(0, 0), (0, 1), (1, 0), (1, 1), (2, 1)]
    , piece4 = mkPiece [(0, 0), (0, 1), (1, 1), (2, 1), (3, 1)]
    , piece5 = mkPiece [(0, 0), (1, 0), (2, 0), (2, 1), (3, 1)]
    , piece6 = mkPiece [(0, 0), (1, 0), (1, 1), (1, 2), (2, 2)]
    , piece7 = mkPiece [(0, 0), (1, 0), (2, 0), (2, 1), (2, 2)]
    }
  where
    mkPiece = PieceRelative . Set.fromList . map Vector

{----- Orientations -----}

-- | All the pieces in the puzzle, in every orientation.
allPieceOrientations :: Pieces [PieceRelative]
allPieceOrientations = nubOrd . map normalizePiece . getAllOrientations <$> allPieces

-- | Get all possible orientations for the given piece.
getAllOrientations :: PieceRelative -> [PieceRelative]
getAllOrientations piece =
  rotatedOrientations piece ++
  rotatedOrientations (flipPiece piece)

-- | Get all orientations for the given piece when rotating it.
rotatedOrientations :: PieceRelative -> [PieceRelative]
rotatedOrientations = take 4 . iterate rotate90
  where
    rotate90 = PieceRelative . Set.map rotateVector . unPieceRelative
    rotateVector (Vector (dx, dy)) = Vector (-dy, dx)

-- | Flip the given piece over the x-axis.
flipPiece :: PieceRelative -> PieceRelative
flipPiece = PieceRelative . Set.map flipVector . unPieceRelative
  where
    flipVector (Vector (dx, dy)) = Vector (dx, -dy)

normalizePiece :: PieceRelative -> PieceRelative
normalizePiece (PieceRelative pieceVectors) = PieceRelative (Set.map normalize pieceVectors)
  where
    (minX, minY) =
      foldr
        (\(Vector (dx, dy)) (x, y) -> (min x dx, min y dy))
        (100, 100)
        pieceVectors

    normalize (Vector (dx, dy)) = Vector (dx - minX, dy - minY)
