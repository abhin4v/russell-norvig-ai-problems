import Data.Ix
import Data.Array
import Data.List
import Data.List.Split
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.PQueue.Prio.Min as PQ
import System.Random
import Control.Monad.State
-- import System.IO.Unsafe (unsafePerformIO)
-- import Debug.Trace (putTraceMsg)

-- trace :: String -> a -> a
-- trace string expr = unsafePerformIO $ do
--     putTraceMsg string
--     return expr

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
swap i i' a = a // [(i, a ! i'), (i', a ! i)]

-- Cost of a move
type Cost = Int

-- A state in the game
class Eq a => GameState a where
  succs ::  a -> [(a, Cost)]

-- A* algorithm: Find a path from initialState to goalState using heuristic
astar :: (GameState a, Show a, Ord a) => a ->  a -> (a ->  a -> Cost) -> [a]
astar initState goalState hueristic =
  astar' (PQ.singleton (hueristic initState goalState) (initState, 0)) S.empty M.empty
  where
    astar' pq seen tracks =
      if state == goalState
      then findPath tracks state
      else if S.member state seen
           then astar' pq' seen tracks
           else astar' pq'' seen' tracks'
      where 
        (state, cost) = snd . PQ.findMin $ pq
        pq' = PQ.deleteMin pq
        seen' =  S.insert state seen
        successors = filter (\(s, _, _) -> not $ S.member s seen') 
                     $ succsWithPrio state cost
        pq'' = foldl (\q (s, c, h) -> PQ.insert (c + h) (s, c) q) 
               pq' successors
        tracks' = foldl (\m (s, _, _) -> M.insert s state m) tracks successors
        
    succsWithPrio state cost =
      map (\(s,c) -> (s, cost + c, hueristic s goalState)) . succs $ state

    findPath tracks state = 
      if M.member state tracks
      then findPath tracks (fromJust . M.lookup state $ tracks) ++ [state]
      else [state]

-- A point in 2d
type Point = (Int, Int)

-- A sliding puzzle
-- blank : which item is considered blank
-- pzState : the current state of the puzzle
data Puzzle a = Puzzle { blank :: a, pzState :: Array Point a } deriving (Eq, Ord)

-- Get puzzle size
puzzleSize :: Puzzle a -> Int
puzzleSize = fst . snd . bounds . pzState

-- Create a puzzle give the blank, the puzzle size and the puzzle state as a list,
-- left to right, top to bottom.
-- Return Just puzzle if valid, Nothing otherwise
fromList :: a -> Int -> [a] -> Maybe (Puzzle a)
fromList b n xs =
  if n * n /= length xs
  then Nothing
  else Just . Puzzle b $ array ((1, 1), (n, n)) [((i, j), xs !! (n * (i-1) + (j-1)))
                                                | i <- range (1, n), j <- range (1, n)]

-- Shows the puzzle state as a string
showPuzzleState :: Show a => Puzzle a -> String
showPuzzleState pz =
  ('\n' :) . concat . intersperse "\n"
  . map (concat . intersperse " ") . splitEvery len
  . map show . elems . pzState $ pz
  where len = puzzleSize pz

-- Find the position of the blank
blankPos :: Eq a => Puzzle a -> Point
blankPos pz =
  fst . fromJust . find (\(i, tile) -> tile == (blank pz)) . assocs . pzState $ pz

-- Get the legal neighbouring positions
neighbourPos :: Int -> Point -> [Point]
neighbourPos len p@(x, y) =
  filter (\(x',y') -> and [x' >= 1, y' >= 1, x' <= len, y' <= len])
  [(x+1,y), (x-1,y), (x,y+1), (x,y-1)]

-- Get the next legal puzzle states
nextStates :: Eq a => Puzzle a -> [Puzzle a]
nextStates pz = map (\p -> Puzzle (blank pz) (swap p blankAt (pzState pz)))
                $ neighbourPos len blankAt
  where
    len = puzzleSize pz
    blankAt = blankPos pz

instance Eq a => GameState (Puzzle a) where
  succs pz = zip (nextStates pz) (repeat 1)

instance (Show a) => Show (Puzzle a) where
  show pz = showPuzzleState pz

-- Shuffles a puzzle n times randomly to return a new (reachable) puzzle.
shufflePuzzle :: (Eq a) => Int -> Puzzle a -> RandomState (Puzzle a)
shufflePuzzle n pz =
  if n == 0
  then return pz
  else do
    let s = succs pz
    i <- getRandomR (0, length s - 1)
    shufflePuzzle (n - 1) (fst (s !! i))

-- Calculates the number of inversions in puzzle
inversions :: Ord a => Puzzle a -> Int
inversions pz = sum . map (\l -> length . filter (\e -> e < head l) $ (tail l))
                . filter ((> 1). length) . tails
                . filter (not . (== b)) . elems . pzState $ pz
  where b = blank pz

-- Calculates the puzzle pairty. The puzzle pairty is invariant under legal moves.
puzzlePairty :: (Ord a) => Puzzle a -> Int
puzzlePairty pz =
  if odd w
  then (w + i) `mod` 2
  else (w + i + 1 - b) `mod` 2
  where w = puzzleSize pz
        i = inversions pz
        b = fst . blankPos $ pz

-- Solves an n sliding puzzle from initState to goalState using heuristic.
-- Return Nothing if the goalState is not reachable from initState
-- else returns Just solution.
solvePuzzle :: (Show a, Ord a) => Puzzle a -> Puzzle a
               -> (Puzzle a -> Puzzle a -> Cost) -> Maybe [Puzzle a]
solvePuzzle initState goalState hueristic =
  if puzzlePairty initState /= puzzlePairty goalState
  then Nothing
  else Just (astar initState goalState hueristic)

-- Returns number of tiles in wrong position in givenState compared to goalState
wrongTileCount :: Eq a => Puzzle a -> Puzzle a -> Cost
wrongTileCount givenState goalState =
  length . filter (\(a, b) -> a /= b)
  $ zip (elems . pzState $ givenState) (elems . pzState $ goalState)

manhattanDistance :: Point -> Point -> Int
manhattanDistance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

sumManhattanDistance :: Ord a => Puzzle a -> Puzzle a -> Cost
sumManhattanDistance givenState goalState =
  sum . map (\(p, t) -> manhattanDistance p (fromJust . M.lookup t $ revM)) 
  . assocs . pzState $ givenState
  where
    revM = M.fromList . map (\(x, y) -> (y, x)) . assocs . pzState $ goalState