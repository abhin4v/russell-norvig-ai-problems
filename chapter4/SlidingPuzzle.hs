{-# LANGUAGE BangPatterns #-}

-- Solves the sliding puzzle problem (http://en.wikipedia.org/wiki/Sliding_puzzle)
-- using A* algorithm

module SlidingPuzzle where

import Data.Ix
import qualified Data.Array as A
import Data.Array (Array, array, (//), (!))
import Data.List
import Data.List.Split
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.PQueue.Prio.Min as PQ
import System.Random
import System.Environment
import Control.Monad.State

-- A State with a ramdom generator
type RandomState = State StdGen

-- Generates a random element between given limits inside State monad
getRandomR :: Random a => (a, a) -> RandomState a
getRandomR limits = do
  gen <- get
  let (val, gen') = randomR limits gen
  put gen'
  return val

-- Swap the contents of two array indices i and i' in array a
swap :: Ix a => a -> a -> Array a b -> Array a b
swap i i' a = a // [(i, ai'), (i', ai)]
  where
    !ai' = a ! i'
    !ai = a ! i

-- Cost of a move
type Cost = Int

-- A state in the game
class Ord a => GameState a where
  succs ::  a -> [(a, Cost)]

-- A* algorithm: Find a path from initial state to goal state using heuristic
-- Returns Nothing if no path found. Else returns Just (path cost, path).
astar :: GameState a => a ->  a -> (a ->  a -> Cost) -> Maybe (Cost, [a])
astar initState goalState hueristic =
  astar' (PQ.singleton (hueristic initState goalState) (initState, 0)) S.empty M.empty
  where
    -- pq: open set, seen: closed set, tracks: tracks of states
    astar' pq seen tracks
      -- If goal state reached then construct the path from the tracks and state
      | state == goalState = Just (gcost, findPath tracks state)
      -- If open set is empty then search has failed. Return Nothing
      | PQ.null pq = Nothing
      -- If state has already been seen then discard it and continue
      | S.member state seen = astar' pq' seen tracks
      -- Else expand the state and continue
      | otherwise = astar' pq'' seen' tracks'
      where
        -- Find the state with min f-cost
        !(state, gcost) = snd . PQ.findMin $ pq

        -- Delete the state from open set
        !pq' = PQ.deleteMin pq

        -- Add the state to the closed set
        !seen' =  S.insert state seen

        -- Find the successors (with their g and h costs) of the state
        -- which have not been seen yet
        !successors = filter (\(s, _, _) -> not $ S.member s seen')
                     $ successorsAndCosts state gcost

        -- Insert the successors in the open set
        !pq'' = foldl' (\q (s, g, h) -> PQ.insert (g + h) (s, g) q) pq' successors

        -- Insert the tracks of the successors
        !tracks' = foldl' (\m (s, _, _) -> M.insert s state m) tracks successors

    -- Finds the successors of a given state and their costs
    successorsAndCosts state gcost =
      map (\(s,g) -> (s, gcost + g, hueristic s goalState)) . succs $ state

    -- Constructs the path from the tracks and last state
    findPath tracks state =
      if M.member state tracks
      then findPath tracks (fromJust . M.lookup state $ tracks) ++ [state]
      else [state]

-- A point in 2d
type Point = (Int, Int)

-- A sliding puzzle
-- blank : which item is considered blank
-- blankPos : position of blank
-- pzState : the current state of the puzzle
data Puzzle a = Puzzle { blank :: !a, blankPos :: !Point, pzState :: !(Array Point a) }
              deriving (Eq, Ord)

-- Get puzzle size
puzzleSize :: Puzzle a -> Int
puzzleSize = fst . snd . A.bounds . pzState

-- Create a puzzle give the blank, the puzzle size and the puzzle state as a list,
-- left to right, top to bottom.
-- Return Just puzzle if valid, Nothing otherwise
fromList :: Ord a => a -> Int -> [a] -> Maybe (Puzzle a)
fromList b n xs =
  if (n * n /= length xs) || (b `notElem` xs)
  then Nothing
  else Just Puzzle { blank = b
                   , blankPos = let (d, r) = (fromJust . elemIndex b $ xs) `divMod` n
                                in (d + 1, r + 1)
                   , pzState = array ((1, 1), (n, n))
                               [((i, j), xs !! (n * (i - 1) + (j - 1)))
                               | i <- range (1, n), j <- range (1, n)]
                   }

-- Shows the puzzle state as a string
showPuzzleState :: Show a => Puzzle a -> String
showPuzzleState pz =
  ('\n' :) . intercalate "\n"
  . map unwords . splitEvery (puzzleSize pz)
  . map show . A.elems . pzState $ pz

-- Get the legal neighbouring positions
neighbourPos :: Int -> Point -> [Point]
neighbourPos len p@(x, y) =
  filter (\(x',y') -> and [x' >= 1, y' >= 1, x' <= len, y' <= len])
  [(x+1,y), (x-1,y), (x,y+1), (x,y-1)]

-- Get the next legal puzzle states
nextStates :: Ord a => Puzzle a -> [Puzzle a]
nextStates pz = map (\p -> Puzzle (blank pz) p (swap p blankAt (pzState pz)))
                $ neighbourPos (puzzleSize pz) blankAt
  where
    blankAt = blankPos pz

-- Make Puzzle an instance of GameState with unit step cost
instance Ord a => GameState (Puzzle a) where
  succs pz = zip (nextStates pz) (repeat 1)

-- Make Puzzle an instance of Show for pretty printing
instance Show a => Show (Puzzle a) where
  show = showPuzzleState

-- Shuffles a puzzle n times randomly to return a new (reachable) puzzle.
shufflePuzzle :: Ord a => Int -> Puzzle a -> RandomState (Puzzle a)
shufflePuzzle n pz =
  if n == 0
  then return pz
  else do
    let s = nextStates pz
    i <- getRandomR (0, length s - 1)
    shufflePuzzle (n - 1) (s !! i)

-- Calculates the number of inversions in puzzle
inversions :: Ord a => Puzzle a -> Int
inversions pz = sum . map (\l -> length . filter (\e -> e < head l) $ tail l)
                . filter ((> 1). length) . tails
                . filter (not . (== blank pz)) . A.elems . pzState $ pz

-- Calculates the puzzle pairty. The puzzle pairty is invariant under legal moves.
puzzlePairty :: Ord a => Puzzle a -> Int
puzzlePairty pz =
  if odd w
  then (w + i) `mod` 2
  else (w + i + 1 - b) `mod` 2
  where w = puzzleSize pz
        i = inversions pz
        b = fst . blankPos $ pz

-- Solves a sliding puzzle from initial state to goal state using the given heuristic.
-- Return Nothing if the goal state is not reachable from initial state
-- else returns Just (cost, path).
solvePuzzle ::  Ord a => Puzzle a -> Puzzle a
               -> (Puzzle a -> Puzzle a -> Cost) -> Maybe (Cost, [Puzzle a])
solvePuzzle initState goalState hueristic =
  if puzzlePairty initState /= puzzlePairty goalState
  then Nothing
  else astar initState goalState hueristic

-- Returns number of tiles in wrong position in given state compared to goal state
wrongTileCount :: Ord a => Puzzle a -> Puzzle a -> Cost
wrongTileCount givenState goalState =
  length . filter (uncurry (/=))
  $ zip (A.elems . pzState $ givenState) (A.elems . pzState $ goalState)

-- Calculates Manhattan distance between two points
manhattanDistance :: Point -> Point -> Int
manhattanDistance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

-- Calculates the sum of Manhattan distances of tiles between positions in
-- given state and goal state
sumManhattanDistance :: Ord a => Puzzle a -> Puzzle a -> Cost
sumManhattanDistance givenState goalState =
  sum . map (\(p, t) -> manhattanDistance p (fromJust . M.lookup t $ revM))
  . A.assocs . pzState $ givenState
  where
    revM = M.fromList . map (\(x, y) -> (y, x)) . A.assocs . pzState $ goalState

-- The classic 15 puzzle (http://en.wikipedia.org/wiki/Fifteen_puzzle)

fifteenPuzzle = nPuzzle 4 50

-- seed : the seed for random generator
nPuzzle :: Int -> Int -> Int -> IO ()
nPuzzle n shuffles seed = do
  -- Random generator
  let gen = mkStdGen seed

  -- The goal
  let goalState = fromJust $ fromList 0 n [0 .. (n * n -1)]
  -- Shuffle the goal to get a random puzzle state
  let initState = evalState (shufflePuzzle shuffles goalState) gen
  -- Solve using sum manhattan distance heuristic
  let (cost, solution) = fromJust $ solvePuzzle initState goalState sumManhattanDistance

  -- Print the solution
  forM_ solution $ \s -> print s

  putStrLn ("Cost: " ++ show cost)

-- The main
main :: IO ()
main = do
  args <- fmap (map read) getArgs
  nPuzzle (args !! 0) (args !! 1) (args !! 2)
