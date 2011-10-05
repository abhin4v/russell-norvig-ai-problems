{-# LANGUAGE FlexibleContexts, TemplateHaskell #-}

module AI.Vacuum.Cleaner where

import AI.Vacuum.Grid
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.State
import Prelude hiding (id, (.))
import Control.Category
import Data.Maybe (fromJust)
import Data.Lens.Common
import Data.Lens.Template

data Percept = TouchSensor | PhotoSensor | InfraredSensor deriving (Eq, Ord, Show)
type Percepts = [Percept]
type PerceptsHistory = [Percepts]
data Action = GoForward | TurnRight | TurnLeft | SuckDirt | TurnOff deriving (Eq, Show)
data CleanerState = On | Off deriving (Eq, Show)
type Score = Int
data Cleaner = Cleaner {
  _state :: CleanerState,
  _cell :: Cell,
  _direction :: Direction,
  _path :: [Point],
  _perceptsHist :: PerceptsHistory,
  _actionHist :: [Action],
  _score :: Score
  } deriving (Show)

makeLenses [''Cleaner]

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

performance :: Cleaner -> Grid -> Float
performance cleaner grid =
  100 * fromIntegral (cleaner^.score) 
  / fromIntegral (99 * dirtCellCount grid - cellCount grid)
  where
    dirtCellCount = M.size . M.filter ((== Dirt) . (cellType ^$))
    cellCount = M.size
    
coverage :: Cleaner -> Grid -> Float
coverage cleaner grid =
  100 * fromIntegral (S.size . S.fromList $ cleaner^.path)
  / fromIntegral (M.size grid)

cleanerAtHome :: Cleaner -> Grid -> Bool
cleanerAtHome cleaner grid = 
  (== Home) . (cellType ^$) . fromJust . (flip lookupCell $ grid) . head $ cleaner^.path

