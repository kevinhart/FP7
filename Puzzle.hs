-- | A class with a backtracking algorithm
module Puzzle
where

-- | @Puzzle@ is a class of puzzles solved by brute-force backtracking.
-- The class requires 'solved' and 'choices'
-- and provides 'solutions' and 'solve'.
class Eq p => Puzzle p where
  -- | @solved puzzle@ is true if the puzzle is solved.
  solved :: p -> Bool
  -- | @choices puzzle@ returns a list of possible new puzzles if the
  -- puzzle is not solved.
  choices :: p -> [p]

  -- | @solve puzzle@ returns the first solution of a puzzle, if any.
  solve :: p -> p
  -- | @solutions puzzle@ returns a list of all solutions for a puzzle.
  solutions :: p -> [p]
