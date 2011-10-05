{-# LANGUAGE FlexibleContexts #-}

module AI.Vacuum where

import qualified Data.Map as M
import Control.Monad.State
import Data.Maybe (isJust, isNothing, fromJust)
import System.IO.Unsafe (unsafePerformIO)
import Debug.Trace (putTraceMsg)

trace :: String -> a -> a
trace string expr = unsafePerformIO $ do
    putTraceMsg string
    return expr

data Direction = North | East | South | West deriving (Eq, Show, Enum, Bounded)
data Percept = TouchSensor | PhotoSensor | InfraredSensor deriving (Eq, Ord, Show)
type Percepts = [Percept]
type PerceptsHistory = [Percepts]
data Action = GoForward | TurnRight | TurnLeft | SuckDirt | TurnOff deriving (Eq, Show)
data CellType = Empty | Furniture | Dirt | Home deriving (Eq, Show)

type Point = (Int, Int)
data Cell = Cell Point CellType deriving (Eq, Show)
type Grid = M.Map Point Cell

data CleanerState = On | Off deriving (Eq, Show)
type Score = Int
data Cleaner =
  Cleaner {
    clState :: CleanerState,
    clCell :: Cell,
    clDir :: Direction,
    clPrcptsHist :: PerceptsHistory,
    clScore :: Score
    } deriving (Show)

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

cell :: Point -> Grid -> Maybe Cell
cell = M.lookup

forwardCell :: Cell -> Direction -> Grid -> Maybe Cell
forwardCell (Cell point _) dir grid = cell (forwardPoint point dir) grid

rightCell :: Cell -> Direction -> Grid -> Maybe Cell
rightCell (Cell point _) dir grid = cell (rightPoint point dir) grid

leftCell :: Cell -> Direction -> Grid -> Maybe Cell
leftCell (Cell point _) dir grid = cell (leftPoint point dir) grid

gridFromCellList :: [Cell] -> Grid
gridFromCellList = foldl (\m cell@(Cell p _) -> M.insert p cell m) M.empty

createCleaner :: Cell -> Direction -> Cleaner
createCleaner cell dir = Cleaner On cell dir [] 0

turnRight :: (MonadState Grid m) => Cleaner -> m Cleaner
turnRight (Cleaner state cell dir ph score) =
  return $ Cleaner state cell (right dir) ([] : ph) score

turnLeft :: (MonadState Grid m) => Cleaner -> m Cleaner
turnLeft (Cleaner state cell dir ph score) =
  return $ Cleaner state cell (left dir) ([] : ph) score

moveForward :: (MonadState Grid m) => Cleaner -> m Cleaner
moveForward cleaner@(Cleaner state cell@(Cell _ cellType) dir ph score) = do
  grid <- get
  return $
    case forwardCell cell dir grid of
      Nothing -> Cleaner state cell dir ([TouchSensor] : ph) score
      Just nextCell@(Cell _ nextCellType) ->
        case nextCellType of
          Empty -> Cleaner state nextCell dir ([] : ph) score
          Furniture -> Cleaner state cell dir ([TouchSensor] : ph) score
          Dirt -> Cleaner state nextCell dir ([PhotoSensor] : ph) score
          Home -> Cleaner state nextCell dir ([InfraredSensor] : ph) score

doAction :: (MonadState Grid m) => Action -> Cleaner -> m Cleaner
doAction action cleaner@(Cleaner state cell@(Cell point cellType) dir ph score) =
  case action of
    GoForward -> moveForward $ Cleaner state cell dir ph (score - 1)
    TurnRight -> turnRight $ Cleaner state cell dir ph (score - 1)
    TurnLeft -> turnLeft $ Cleaner state cell dir ph (score - 1)
    SuckDirt ->
      case cellType of
        Dirt -> do
          grid <- get
          put $ M.insert point (Cell point Empty) grid
          return $ Cleaner state cell dir ([] : ph) (score + 99)
        otherwise -> return $ Cleaner state cell dir ([] : ph) (score - 1)
    TurnOff ->
      case cellType of
        Home -> return $ Cleaner Off cell dir ([] : ph) score
        otherwise -> return $ Cleaner Off cell dir ([] : ph) (score - 1000)

