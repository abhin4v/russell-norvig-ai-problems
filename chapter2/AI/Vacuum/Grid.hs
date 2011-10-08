{-# LANGUAGE TemplateHaskell #-}

module AI.Vacuum.Grid where

import qualified Data.Map as M
import qualified Data.List as L
import Data.Maybe (fromJust)
import Data.Ix (range)
import Control.Monad (forM_)
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
type Path = [Point]
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

backwardPoint :: Point -> Direction -> Point
backwardPoint (x, y) North = (x, y + 1)
backwardPoint (x, y) East = (x - 1, y)
backwardPoint (x, y) South = (x, y - 1)
backwardPoint (x, y) West = (x + 1, y)

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

orientation :: Point -> Point -> (Maybe Direction, Maybe Direction)
orientation from@(x1, y1) to@(x2, y2)
  | from == to = (Nothing, Nothing)
  | y1 == y2 && x2 > x1 = (Just East, Nothing)
  | y1 == y2 && x2 < x1 = (Just West, Nothing)
  | x1 == x2 && y2 > y1 = (Nothing, Just South)
  | x1 == x2 && y2 < y1 = (Nothing, Just North)
  | y2 < y1 && x2 > x1 = (Just East, Just North)
  | y2 < y1 && x2 < x1 = (Just West, Just North)
  | y2 > y1 && x2 > x1 = (Just East, Just South)
  | y2 > y1 && x2 < x1 = (Just West, Just South)

horzPath :: Point -> Point -> [Point]
horzPath p1@(x1, y1) p2@(x2, _)
  | x1 <= x2 = map (\x -> (x, y1)) $ range (x1, x2)
  | otherwise = reverse . map (\x -> (x, y1)) $ range (x2, x1)

vertPath :: Point -> Point -> [Point]
vertPath p1@(x1, y1) p2@(_, y2)
  | y1 <= y2 = map (\y -> (x1, y)) $ range (y1, y2)
  | otherwise = reverse . map (\y -> (x1, y)) $ range (y2, y1)

manhattanPaths :: Point -> Point -> [[Point]]
manhattanPaths p1@(x1,y1) p2@(x2,y2)
  | p1 == p2 = []
  | otherwise = [L.nub (hp1 ++ vp1), L.nub (vp2 ++ hp2)]
    where
      hp1 = horzPath p1 p2
      vp1 = vertPath (last hp1) p2
      vp2 = vertPath p1 p2
      hp2 = horzPath (last vp2) p2

cornerPoints :: Point -> Int -> [Point]
cornerPoints (x,y) distance =
  [(x + distance, y + distance),
   (x - distance, y + distance),
   (x - distance, y - distance),
   (x + distance, y - distance)]

borderingPoints :: Point -> Int -> [Point]
borderingPoints point distance =
  L.nub . concat
  . map (\(p1@(_,y1), p2@(_,y2)) ->
          if y1 == y2 then horzPath p1 p2 else vertPath p1 p2)
  . pairs . take 5 . cycle $ cornerPoints point distance

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

freqMap :: (Ord a) => [a] -> [(a, Int)]
freqMap = M.toList . foldl (\m t -> M.insertWith (+) t 1 m) M.empty

pairs :: [a] -> [(a,a)]
pairs [] = []
pairs [_] = []
pairs (x1 : x2 : xs) = (x1, x2) : pairs (x2 : xs)

gridWidth :: Grid -> Int
gridWidth = (+ 1) . maximum . map fst . M.keys

gridHeight :: Grid -> Int
gridHeight = (+ 1) . maximum . map snd . M.keys

gridStats :: Grid -> [(CellType, Int)]
gridStats = freqMap . map (cellType ^$) . M.elems

showCell :: Cell -> String
showCell cell =
   case cell^.cellType of
     Dirt -> "X "
     Empty -> "O "
     Furniture -> "F "
     Home -> "H "

printGrid :: Grid -> IO ()
printGrid grid = do
  let width = gridWidth grid
  let height = gridHeight grid

  forM_ (range (0, height - 1)) $ \y -> do
    forM_ (range (0, width - 1)) $ \x ->
      putStr . showCell . fromJust . lookupCell (x,y) $ grid
    putStrLn ""
