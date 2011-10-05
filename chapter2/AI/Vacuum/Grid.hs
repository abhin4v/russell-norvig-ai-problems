{-# LANGUAGE TemplateHaskell #-}

module AI.Vacuum.Grid where

import qualified Data.Map as M
import Data.Lens.Common
import Data.Lens.Template
import System.IO.Unsafe (unsafePerformIO)
import Debug.Trace (putTraceMsg)

trace :: String -> a -> a
trace string expr = unsafePerformIO $ do
    putTraceMsg string
    return expr

data Direction = North | East | South | West deriving (Eq, Show, Enum, Bounded)
data CellType = Empty | Furniture | Dirt | Home deriving (Eq, Show, Ord)
type Point = (Int, Int)
data Cell = Cell { _point :: Point, _cellType :: CellType } deriving (Eq, Show)
type Grid = M.Map Point Cell

makeLenses [''Cell]

class (Enum a, Eq a, Bounded a) => WrappedBoundedEnum a where
  next :: a -> a
  prev :: a -> a

  next x = if (maxBound == x) then minBound else succ x
  prev x = if (minBound == x) then maxBound else pred x

instance WrappedBoundedEnum Direction

right :: Direction -> Direction
right = next

left :: Direction -> Direction
left = prev

forwardPoint :: Point -> Direction -> Point
forwardPoint (x, y) North = (x, y - 1)
forwardPoint (x, y) East = (x + 1, y)
forwardPoint (x, y) South = (x, y + 1)
forwardPoint (x, y) West = (x - 1, y)

rightPoint :: Point -> Direction -> Point
rightPoint (x, y) North = (x + 1, y)
rightPoint (x, y) East = (x, y + 1)
rightPoint (x, y) South = (x - 1, y)
rightPoint (x, y) West = (x, y - 1)

leftPoint :: Point -> Direction -> Point
leftPoint (x, y) North = (x -1, y)
leftPoint (x, y) East = (x, y - 1)
leftPoint (x, y) South = (x + 1, y)
leftPoint (x, y) West = (x, y + 1)

lookupCell :: Point -> Grid -> Maybe Cell
lookupCell = M.lookup

forwardCell :: Cell -> Direction -> Grid -> Maybe Cell
forwardCell (Cell point _) = lookupCell . (forwardPoint point)

rightCell :: Cell -> Direction -> Grid -> Maybe Cell
rightCell (Cell point _) = lookupCell . (rightPoint point)

leftCell :: Cell -> Direction -> Grid -> Maybe Cell
leftCell (Cell point _) = lookupCell . (leftPoint point)

gridFromCellList :: [Cell] -> Grid
gridFromCellList = foldl (\m cell@(Cell p _) -> M.insert p cell m) M.empty

gridWidth :: Grid -> Int
gridWidth = (+ 1) . maximum . map fst . M.keys

gridHeight :: Grid -> Int
gridHeight = (+ 1) . maximum . map snd . M.keys

gridStats :: Grid -> [(CellType, Int)]
gridStats = 
  M.toList . foldl (\m t -> M.insertWith (+) t 1 m) M.empty . map (cellType ^$) . M.elems