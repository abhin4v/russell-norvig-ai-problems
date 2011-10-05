module AI.Vacuum.RandomGrid where

import AI.Vacuum
import System.Random
import Control.Monad.State
import qualified Data.Map as M
import Data.Ix (range)

-- Implement an environment for a n X m rectangular room, where each square has a 5% chance
-- of containing dirt, and n and m are chosen at random from the range 8 to 15, inclusive.

type RandomState = State StdGen

getRandomR :: Random a => (a, a) -> RandomState a
getRandomR limits = do
  gen <- get
  let (val, gen') = randomR limits gen
  put gen'
  return val

makeCell :: Point -> Float -> RandomState Cell
makeCell point dirtProb = do
  dirtR <- getRandomR (0.0, 1.0)
  if dirtR <= dirtProb
    then return $ Cell point Dirt
    else return $ Cell point Empty

makeGrid :: (Int, Int) -> (Int, Int) -> Float -> RandomState Grid
makeGrid minMaxWidth minMaxHeight dirtProb = do
  width <- getRandomR minMaxWidth
  height <- getRandomR minMaxHeight
  foldM
    (\m p -> makeCell p dirtProb >>= \c -> return $ M.insert p c m)
    (M.singleton (0,0) (Cell (0,0) Home))
    [(x,y) | x <- range (0, width - 1), y <- range (0, height -1), (x,y) /= (0,0)]
