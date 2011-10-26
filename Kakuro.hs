-- | A brute-force backtracking solver for Kakuro puzzles.
module Kakuro
( Kakuro
, Word
--  , across
--  , down
--  , make
) where

import Data.Array
import Data.List (nub,intersect,(\\))
import Puzzle

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

-- implementing Show for Kakuro
showKakuro :: Kakuro -> String
showKakuro k = showRow (columns k) (grid k)
  where
    showDigit n | n == 0         = "?"
                | n < 0 || n > 9 = " "
                | otherwise      = show n
    showRow cols [] = ""
    showRow cols list = (foldl (++) "" (map showDigit (take cols list)))
                        ++ "\n" ++ (showRow cols (drop cols list))

instance Show (Kakuro) where
  show x = showKakuro x

{- | @Word@ represents one crossword in a Kakuro puzzle as the sum, a vector of
     digits which can be part of the sum, and a vector of indices to cell
     contents in the Kakuro puzzle.
-}
data Word = Word { mySum :: Int,
                   myDigits :: [Int],
                   indices :: [Int] } deriving (Eq, Show)

validDigits :: [Int] -> Word -> [Int]
validDigits grid (Word wsum _ indices) = nub . concat $ combos
  where
    combos = [ s \\ chosenDigits | s <- (sums!((length indices),wsum)), all (`elem` s) chosenDigits ]
    chosenDigits = [ x | x <- map (grid!!) indices, x /= 0]

-- | @bits number@ returns a list of bit positions (1..9) which are set in a
--   number (3..511)
bits :: Int -> [Int]
bits n | n <= 0         = []
       | n `mod` 2 /= 0 = 1:rembits
       | otherwise      = rembits
           where rembits = map (+1) $ bits (n `div` 2)

{- | @sums@ is an array which contains for a length (1..9) and a sum (1..45)
     lists of unique digits which add up to the sum.  It is computed by
     interpreting the bits in the numbers 3.511.
-}
sums :: Array (Int, Int) [[Int]]
sums = accumArray (flip (:)) [] ((1,1),(9,45))
         [ ((length combo, sum combo), combo) | combo <- (map bits [3..511])]

{- | @digits@ is an array which contains for a length (1..9) and a sum (1..45)
     a list of unique digits from which all lists must be composed which add up
     to the sum.  It is computed by flattening the actual lists contained in
     'sums'.
-}
digits :: Array (Int, Int) [Int]
digits = fmap (nub . concat) sums

instance Puzzle Kakuro where
  solved (Kakuro grid _ _ _) = all (/=0) grid

  choices k@(Kakuro masterGrid _ _ _) = choicesR k
    where
     choicesR (Kakuro (g:gs) c words (l:ls))
      | g /= 0    = map restore $ choicesR (Kakuro gs c words ls)
      | otherwise = [Kakuro (guess:gs) c words (l:ls) | guess <- possibilities ]
          where
            restore (Kakuro gs c ws ls) = Kakuro (g:gs) c ws (l:ls)
            possibilities = foldr intersect [1..9] digitslist
            digitslist = map ((validDigits masterGrid) . (words!!)) l
