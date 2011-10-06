module AI.Vacuum.ReflexAgent where

import AI.Vacuum.Cleaner
import AI.Vacuum.Grid
import AI.Vacuum.RandomGrid
import Data.Lens.Common
import Control.Monad.State
import System.Random
import System (getArgs)
import Data.Maybe (fromJust)

chooseAction :: Percepts -> RandomState Action
chooseAction percepts
  | PhotoSensor `elem` percepts = return SuckDirt
  | InfraredSensor `elem` percepts = return TurnOff
  | TouchSensor `elem` percepts = do
    r <- getRandomR (True, False)
    if r
      then return TurnLeft
      else return TurnRight
  | otherwise = do
    r <- getRandomR ((0.0, 1.0) :: (Float, Float))
    chooseRandom r
    where
      chooseRandom r
        | r < 0.1 = return TurnRight
        | r < 0.2 = return TurnLeft
        | otherwise = return GoForward

runCleaner :: Int -> Cleaner -> StateT Grid RandomState Cleaner
runCleaner turnsLeft cleaner =
  if turnsLeft == 1
    then do
    cleaner' <- doAction TurnOff cleaner
    return cleaner'
    else do
      let ph = cleaner^.perceptsHist
      cleaner'' <- case ph of
        [] -> do
          cleaner' <- doAction GoForward cleaner
          return cleaner'
        _ -> do
          action <- lift $ chooseAction (head ph)
          cleaner' <- doAction action cleaner
          return cleaner'

      case cleaner''^.state of
        Off -> return cleaner''
        On -> runCleaner (turnsLeft - 1) cleaner''

simulateOnGrid :: Int -> Grid -> StdGen -> (Cleaner, Grid)
simulateOnGrid maxTurns grid gen =
  evalState (runStateT (runCleaner maxTurns cleaner) grid) gen
  where cleaner = createCleaner (fromJust $ lookupCell (0,0) grid) East

main :: IO ()
main = do
  gen <- newStdGen
  args <- getArgs
  let minSize = (read $ args !! 0) :: Int
  let maxSize = (read $ args !! 1) :: Int
  let dirtProb = (read $ args !! 2) :: Float
  let maxTurns = (read $ args !! 3) :: Int
  let toPrintGrid = (read $ args !! 4) :: Bool

  let grid = evalState
             (makeRandomGrid (minSize,maxSize) (minSize,maxSize) dirtProb 0.0) gen

  when toPrintGrid $ do
    putStrLn "Grid before traversal"
    printGrid grid
    putStrLn ""

  let (cleaner, grid') = simulateOnGrid maxTurns grid gen

  when toPrintGrid $ do
    putStrLn "Grid after traversal"
    printPath cleaner grid'
    putStrLn ""

  printRunStats cleaner grid
