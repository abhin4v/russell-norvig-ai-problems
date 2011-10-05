module AI.Vacuum.TableLookupAgent where

import AI.Vacuum.Cleaner
import AI.Vacuum.Grid
import Control.Monad.State
import Data.Lens.Common
import Data.Maybe (isJust, isNothing, fromJust)

-- Problem 2.6

possiblePerceptsHistories :: [PerceptsHistory]
possiblePerceptsHistories = takeWhile ((<= 9) . length) $
  [[]] : [[PhotoSensor]] : concatMap (\s -> [[] : s, [PhotoSensor] : s]) possiblePerceptsHistories

-- 0 -> t
-- 1 -> s
-- t -> 0 -> m | 1 -> X
-- s -> 0 -> t | 1 -> X
-- m -> 0 -> t | 1 -> s
-- X -> X

chooseAction :: PerceptsHistory -> Maybe Action
chooseAction ph =
  case ph of
    [] -> Just GoForward
    [[]] -> Just TurnRight
    [[PhotoSensor]] -> Just SuckDirt
    (p:ps) ->
      case lookup ps perceptsHistoryToActionMap of
        Just (Just prevAction) -> chooseAction' ph prevAction
        _ -> Nothing

chooseAction' :: PerceptsHistory -> Action -> Maybe Action
chooseAction' ph prevAction
  | prevAction == TurnRight || prevAction == TurnLeft =
    case head ph of
      [] -> Just GoForward
      [PhotoSensor] -> Nothing
  | prevAction == SuckDirt =
    case head ph of
      [] -> Just TurnRight
      [PhotoSensor] -> Nothing
  | prevAction == GoForward =
    case head ph of
      [] -> Just TurnRight
      [PhotoSensor] -> Just SuckDirt
  | prevAction == TurnOff = error "Cannot move after turnoff"

perceptsHistoryToActionMap :: [(PerceptsHistory, Maybe Action)]
perceptsHistoryToActionMap =
  map (\ph -> (ph, chooseAction ph)) possiblePerceptsHistories

grid1 = gridFromCellList [
  Cell (0, 0) Home, Cell (1, 0) Empty,
  Cell (0, 1) Empty, Cell (1, 1) Empty
  ]

grid2 = gridFromCellList [
  Cell (0, 0) Home, Cell (1, 0) Dirt,
  Cell (0, 1) Empty, Cell (1, 1) Empty
  ]

grid3 = gridFromCellList [
  Cell (0, 0) Home, Cell (1, 0) Empty,
  Cell (0, 1) Dirt, Cell (1, 1) Empty
  ]

grid4 = gridFromCellList [
  Cell (0, 0) Home, Cell (1, 0) Empty,
  Cell (0, 1) Empty, Cell (1, 1) Dirt
  ]

grid5 = gridFromCellList [
  Cell (0, 0) Home, Cell (1, 0) Dirt,
  Cell (0, 1) Dirt, Cell (1, 1) Empty
  ]

grid6 = gridFromCellList [
  Cell (0, 0) Home, Cell (1, 0) Empty,
  Cell (0, 1) Dirt, Cell (1, 1) Dirt
  ]

grid7 = gridFromCellList [
  Cell (0, 0) Home, Cell (1, 0) Dirt,
  Cell (0, 1) Empty, Cell (1, 1) Dirt
  ]

runCleaner :: Cleaner -> State Grid Cleaner
runCleaner cleaner = do
  case chooseAction $ cleaner^.perceptsHist of
    Just action -> do
      cleaner' <- doAction action cleaner
      if InfraredSensor `elem` (head $ cleaner'^.perceptsHist)
        then doAction TurnOff cleaner'
        else runCleaner cleaner'
    _ -> doAction TurnOff cleaner

simulateOnGrid :: Grid -> (Cleaner, Grid)
simulateOnGrid grid =
  runState (runCleaner cleaner) grid
  where cleaner = createCleaner (fromJust $ lookupCell (0, 0) grid) East
