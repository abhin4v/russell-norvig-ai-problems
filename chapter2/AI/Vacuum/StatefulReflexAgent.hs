{-# LANGUAGE FlexibleContexts #-}

module AI.Vacuum.StatefulReflexAgent where

import AI.Vacuum.Grid
import AI.Vacuum.Cleaner hiding (doAction)
import qualified AI.Vacuum.Cleaner (doAction)
import AI.Vacuum.RandomGrid
import qualified Data.Map as M
import qualified Data.List as L
import Data.Maybe (fromMaybe, fromJust)
import Data.Lens.Common
import Data.Ix (range)
import Control.Monad.State
import System.Random
import System (getArgs)

data PointState = Unreachable | Explored | Unexplored deriving (Eq, Ord, Show)
type GridState = M.Map Point PointState

updateGridState :: Point -> PointState -> GridState -> GridState
updateGridState point pointState gridState =
  let gridState' = M.insert point pointState gridState in
  case point of
    (0, 0) ->
      foldl (\m p -> M.insert p Unreachable m) gridState' [(-1, 0), (0, -1), (-1, -1)]
    (0, y) -> M.insert (-1, y) Unreachable gridState'
    (x, 0) -> M.insert (x, -1) Unreachable gridState'
    _ -> gridState'

createGridState :: Cleaner -> GridState
createGridState cleaner = updateGridState ((cleaner^.cell)^.point) Explored M.empty

getPointState point = fromMaybe Unexplored . M.lookup point

getCellState :: (Point -> Direction -> Point)
                 -> Cell -> Direction -> GridState -> PointState
getCellState pointFn cell direction = getPointState (pointFn (cell^.point) direction)

leftCellState :: Cell -> Direction -> GridState -> PointState
leftCellState = getCellState leftPoint

rightCellState :: Cell -> Direction -> GridState -> PointState
rightCellState = getCellState rightPoint

forwardCellState :: Cell -> Direction -> GridState -> PointState
forwardCellState = getCellState forwardPoint

backwardCellState :: Cell -> Direction -> GridState -> PointState
backwardCellState = getCellState backwardPoint

doAction :: (MonadState Grid m) => Action -> Cleaner -> GridState -> m (Cleaner, GridState)
doAction action cleaner gridState = do
  cleaner' <- AI.Vacuum.Cleaner.doAction action cleaner
  let gridState' =
        if action == GoForward
        then if TouchSensor `elem` (head (cleaner'^.perceptsHist))
             then updateGridState (nextPoint cleaner') Unreachable gridState
             else updateGridState ((cleaner'^.cell)^.point) Explored gridState
        else gridState
  return (cleaner', gridState')
  where
    nextPoint cl = forwardPoint ((cl^.cell)^.point) (cl^.direction)

possibleManhattanPaths :: [Path] -> GridState -> [Path]
possibleManhattanPaths paths gridState
  | paths == [] = []
  | otherwise =
    filter (L.all (== Explored)
            . map (\p -> getPointState p gridState)
            . filter (\p -> p `notElem` [p1, p2]))
    paths
    where
      p1 = head . head $ paths
      p2 = last . head $ paths

nearestUnexploredPoint :: Point -> Int -> GridState -> Maybe (Point, Path)
nearestUnexploredPoint point maxDist gridState = do
  np <- L.find (\(p,ps) -> ps == Unexplored && pathExists p)
           $ map (\p -> (p, getPointState p gridState))
           $ concatMap (borderingPoints point) [1..maxDist]
  return ((fst np), (head . paths . fst $ np))
  where
    paths p = possibleManhattanPaths (manhattanPaths point p) gridState
    pathExists p = (/= 0) . length . paths $ p

actionsByRelDirection = [
  ("f", [GoForward]),
  ("l", [TurnLeft, GoForward]),
  ("r", [TurnRight, GoForward]),
  ("b", [TurnLeft, TurnLeft, GoForward])]

relDirectionToDirection relDir dir =
  case relDir of
    "f" -> dir
    "l" -> left dir
    "r" -> right dir
    "b" -> right . right $ dir

moveActions :: (Point, Point) -> Direction -> ([Action], Direction)
moveActions (p1, p2) dir =
  (\rd ->
    (fromJust . lookup rd $ actionsByRelDirection,
     relDirectionToDirection rd dir))
  $ fst
  $ fromJust
  $ L.find (\(d, p) -> p == p2)
  $ zip ["f", "l", "r", "b"]
  $ map (\pfn -> pfn p1 dir)
  $ [forwardPoint, leftPoint, rightPoint, backwardPoint]

pathActions :: Direction -> Path -> [Action]
pathActions dir path =
  fst
  . foldl
  (\(as, d) ps -> let (as', d') = moveActions ps d in (as ++ as', d'))
  ([], dir)
  . pairs
  $ path

chooseActions :: Cleaner -> GridState -> RandomState [Action]
chooseActions cleaner gridState =
  case cleaner^.perceptsHist of
    [] -> return [GoForward]
    (ps : _) | PhotoSensor `elem` ps -> return [SuckDirt]
    (ps : _) | InfraredSensor `elem` ps -> return [TurnOff]
    _ ->
      if length unexplored == 0
       then -- trace ("surrounded at " ++ show ((cleaner^.cell)^.point)) $
        case nearestUnexploredPoint ((cleaner^.cell)^.point) 4 gridState of
          Nothing -> do
            r <- getRandomR ((0.0, 1.0) :: (Float, Float))
            -- trace ("choosing on random: " ++ show r) $
            case r of
              r | r < 0.1 -> return [TurnRight]
              r | r < 0.2 -> return [TurnLeft]
              otherwise -> return [GoForward]
          Just (nPoint, path) -> -- trace ("taking path: " ++ show path) $
            return . pathActions (cleaner^.direction) $ path
      else
        return . fromJust . lookup (fst . head $ unexplored) $ actionsByRelDirection
      where
        gridStates =
          zip ["f", "l", "r", "b"] $
          map (\f -> f (cleaner^.cell) (cleaner^.direction) gridState)
          [forwardCellState, leftCellState, rightCellState, backwardCellState]
        unexplored = filter ((== Unexplored) . snd) gridStates

runCleaner :: Int -> Cleaner -> GridState -> StateT Grid RandomState (Cleaner, GridState)
runCleaner turnsLeft cleaner gridState =
  if turnsLeft == 1
    then do
    (cleaner', gridState') <- doAction TurnOff cleaner gridState
    return (cleaner', gridState')
    else do
    actions <- lift $ chooseActions cleaner gridState
    (cleaner', gridState') <-
      foldM
      (\(cl, gs) a -> do
          (cl', gs') <- doAction a cl gs
          return (cl', gs'))
      (cleaner, gridState)
      actions

    case cleaner'^.state of
      Off -> return (cleaner', gridState')
      On -> runCleaner (turnsLeft - (length actions)) cleaner' gridState'

simulateOnGrid :: Int -> Grid -> StdGen -> (Cleaner, GridState, Grid)
simulateOnGrid maxTurns grid gen =
  let ((cleaner', gridState'), grid') =
        evalState (runStateT (runCleaner maxTurns cleaner gridState) grid) gen in
  (cleaner', gridState', grid')
  where
    cleaner = createCleaner (fromJust $ lookupCell (0,0) grid) East
    gridState = createGridState cleaner

printGridState :: GridState -> Grid -> IO ()
printGridState gridState grid = do
  let width = gridWidth grid
  let height = gridHeight grid

  forM_ (range (-1, height)) $ \y -> do
    forM_ (range (-1, width)) $ \x -> do
      case lookupCell (x,y) $ grid of
        Nothing ->
          case M.lookup (x,y) gridState of
            Nothing -> putStr "! "
            Just Unreachable -> putStr "/ "
        Just cell ->
          case M.lookup (cell^.point) gridState of
            Nothing -> putStr "! "
            Just pointState ->
              case pointState of
                Explored -> putStr "- "
                Unexplored -> putStr "! "
                Unreachable -> putStr "/ "
    putStrLn ""

printSimulation :: Int -> Int -> Int -> Float -> Float -> Bool -> IO ()
printSimulation
  minSize maxSize maxTurns dirtProb furnitureProb toPrintGrid = do
  gen <- newStdGen
  let grid = evalState
             (makeRandomGrid (minSize,maxSize) (minSize,maxSize) dirtProb furnitureProb)
             gen

  when toPrintGrid $ do
    putStrLn "Grid before traversal"
    printGrid grid
    putStrLn ""

  let (cleaner, gridState', grid') = simulateOnGrid maxTurns grid gen

  when toPrintGrid $ do
    putStrLn "Grid after traversal"
    printPath cleaner grid'
    putStrLn ""

  when toPrintGrid $ do
    putStrLn "Grid state"
    printGridState gridState' grid
    putStrLn ""

  printRunStats cleaner grid
  putStrLn ("Grid Exploration stats = "
            ++ (show . freqMap $
                [fromMaybe Unexplored . M.lookup (x, y) $ gridState'
                | x <- range (0, gridWidth grid - 1),
                  y <- range(0, gridHeight grid - 1)]))

main :: IO ()
main = do
  args <- getArgs
  let minSize = (read $ args !! 0) :: Int
  let maxSize = (read $ args !! 1) :: Int
  let dirtProb = (read $ args !! 2) :: Float
  let furnitureProb = (read $ args !! 3) :: Float
  let maxTurns = (read $ args !! 4) :: Int
  let toPrintGrid = (read $ args !! 5) :: Bool

  printSimulation minSize maxSize maxTurns dirtProb furnitureProb toPrintGrid
