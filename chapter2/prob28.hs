module AI.Vacuum.ReflexAgent where

import AI.Vacuum
import AI.Vacuum.RandomGrid
import Data.Lens.Common
import Control.Monad.State
import Control.Monad.Identity
import System.Random
import Data.Maybe (isJust, isNothing, fromJust)

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
