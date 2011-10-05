{-# LANGUAGE FlexibleContexts, TemplateHaskell #-}

module AI.Vacuum where

import qualified Data.Map as M
import Control.Monad.State
import Prelude hiding (id, (.))
import Control.Category
import Data.Maybe (isJust, isNothing, fromJust)
import Data.Lens.Common
import Data.Lens.Template
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
data Cell = Cell { _point :: Point, _cellType :: CellType } deriving (Eq, Show)
type Grid = M.Map Point Cell

data CleanerState = On | Off deriving (Eq, Show)
type Score = Int
data Cleaner =
  Cleaner {
    _state :: CleanerState,
    _cell :: Cell,
    _direction :: Direction,
    _path :: [Point],
    _perceptsHist :: PerceptsHistory,
    _actionHist :: [Action],
    _score :: Score
    } deriving (Show)

makeLenses [''Cell, ''Cleaner]

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

createCleaner :: Cell -> Direction -> Cleaner
createCleaner cell dir = Cleaner On cell dir [cell^.point] [] [] 0

setPercepts percepts = perceptsHist ^%= (percepts :)

turnRight :: (MonadState Grid m) => Cleaner -> m Cleaner
turnRight = return . (direction ^%= right) . (setPercepts [])

turnLeft :: (MonadState Grid m) => Cleaner -> m Cleaner
turnLeft = return . (direction ^%= left) . (setPercepts [])

moveForward :: (MonadState Grid m) => Cleaner -> m Cleaner
moveForward cleaner = do
  grid <- get
  return .
    case forwardCell (cleaner^.cell) (cleaner^.direction) grid of
      Nothing -> setPercepts [TouchSensor]
      Just nextCell@(Cell nextPoint nextCellType) ->
        let setNextCellPoint = (cell ^= nextCell) . (path ^%= (nextPoint :)) in
        case nextCellType of
          Empty -> setNextCellPoint . (setPercepts [])
          Furniture -> setPercepts [TouchSensor]
          Dirt -> setNextCellPoint . (setPercepts [PhotoSensor])
          Home -> setNextCellPoint . (setPercepts [InfraredSensor])
    $ cleaner

suckDirt :: (MonadState Grid m) => Cleaner -> m Cleaner
suckDirt cleaner = do
  let point' = (cleaner^.cell)^.point
  grid <- get
  put $ M.insert point' (Cell point' Empty) grid
  return cleaner

doAction :: (MonadState Grid m) => Action -> Cleaner -> m Cleaner
doAction action cleaner = do
  case action of
    GoForward -> moveForward
    TurnRight -> turnRight
    TurnLeft -> turnLeft
    SuckDirt ->
      (if cellType' == Dirt then suckDirt . (score ^%= (+ 100)) else return)
      . (setPercepts [])
    TurnOff ->
      return . (state ^= Off) . (setPercepts [])
      . (if cellType' == Home then id else score ^%= (subtract 1000))
  . (score ^%= subtract 1)
  . (actionHist ^%= (action :))
  $ cleaner
  where
    cellType' = (cleaner^.cell)^.cellType
