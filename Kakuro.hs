-- | A brute-force backtracking solver for Kakuro puzzles.
module Kakuro
  ( Kakuro
  , Word
--  , across
--  , down
--  , make
  ) where

{- | @Kakuro@ represents a Kakuro puzzle, i.e., a crossword puzzle with
     digits, as a vector of cell contents (0: not yet decided, 1..9: decided,
     other: not a cell), a vector which for each cell contains a list of
     indices of one or two words to which the cell belongs, and a vector of
     word descriptions.
-}
data Kakuro = Kakuro { grid :: [Int],
                       columns :: Int,
                       words :: [Word],
                       links :: [[Int]] } deriving Eq

{- | @Word@ represents one crossword in a Kakuro puzzle as the sum, a vector of
     digits which can be part of the sum, and a vector of indices to cell
     contents in the Kakuro puzzle.
-}
data Word = Word { sum :: Int,
                   myDigits :: [Int],
                   indices :: [Int] } deriving (Eq, Show)

-- | @bits number@ returns a list of bit positions (1..9) which are set in a
--   number (3..511)
bits :: Int -> [Int]
bits n | n <= 0         = []
       | n `mod` 2 /= 0 = 1:rembits
       | otherwise      = rembits
           where rembits = map (+1) $ bits (n `div` 2)
