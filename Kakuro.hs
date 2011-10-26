-- | A brute-force backtracking solver for Kakuro puzzles.
module Kakuro
( Kakuro
, Word
, across
, down
, make
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

-- validDigits returns a list of digits that would be valid guesses for the
-- next digit of the sum given the already guessed digits.
-- This is done by querying the digits array for the digits which could go
-- into a subset of the Word.  That is, decompositions shortened by the the
-- previously guessed digits.
validDigits :: [Int] -> Word -> [Int]
validDigits grid (Word wsum _ indices) = digits ! (backlength,backsum)
  \\ chosenDigits -- subtract the already guessed digits to avoid duplicates
    where
      backlength = (length indices) - (length chosenDigits)
      backsum = wsum - (sum chosenDigits)
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
         [ ((length combo, sum combo), combo) | combo <- (map bits [1..511])]

{- | @digits@ is an array which contains for a length (1..9) and a sum (1..45)
     a list of unique digits from which all lists must be composed which add up
     to the sum.  It is computed by flattening the actual lists contained in
     'sums'.
-}
digits :: Array (Int, Int) [Int]
digits = fmap (nub . concat) sums

{- | @across row column sum length@ is a shorthand for a horizontal word. -}
across :: Int -> Int -> Int -> Int -> [Int]
across row column sum length = [0, row, column, sum, length]

{- | @down column row sum length@ is a shorthand for a vertical word. -}
down :: Int -> Int -> Int -> Int -> [Int]
-- note that the column and row arguments are to be reversed since they are
-- accepted in an order that does not fit the pattern of the to-be-returned list
down column row sum length = [1, row, column, sum, length]

{- | @make [ shorthands ]@ returns a Kakuro described by a list of across and
     down.
-}
make :: [[Int]] -> Kakuro
make list = Kakuro grid columns words links
  where
    -- grid composed from links: if there is a link, start with a o (undecided),
    -- otherwise use a -1 to indicate nothing goes there
    grid = [ if length (links!!x) == 0 then -1 else 0 | x <- [0..(columns*rows)-1] ]
    
    -- compute number of columns/rows in the puzzle based on the maximum 
    -- dimensions required by all words
    columns = maximum $ map (\x -> if x!!0 == 0 then (x!!2)+(x!!4)-1 else 1) list
    rows = maximum $ map (\x -> if x!!0 == 1 then (x!!1)+(x!!4)-1 else 1) list
    
    -- makeWords returns a list of (index,word) pairs, in order to facilitate
    -- creations of links (see below)    
    makeWords [] = []
    makeWords ([orient, row, col, sum, length]:xs) =
      [ (index,word) | index <- (indices orient) ] ++ (makeWords xs)
        where
          indices 0 = [ ((row-1)*columns)+(n-1) | n <- [col..(col+length-1)] ]
          indices _ = [ ((n-1)*columns)+(col-1) | n <- [row..(row+length-1)] ]
          word = (Word sum (digits!(length, sum)) (indices orient))
    
    -- wordsPair stores the result of calling makeWords on the input list
    wordsPair = makeWords list
    
    -- words for the Kakuro are obtained by extracting the second part of each
    -- pair (note that this is admittedly sub-optimal since there will be
    -- duplicate words, but it makes it easy to create links from the indices in
    -- this array 
    words = (map snd wordsPair)
    
    -- links is created by first mapping wordsPair (see above) to pairs of
    -- (gridIndex, [wordIndex]) and then combining all [wordIndex] arrays
    -- with the same index in order to give the "links" (i.e. indices into the
    -- words array) for each grid cell
    links = [ (arr!a) | a <- [fst $ bounds arr .. snd $ bounds arr] ]
      where
        arr = accumArray (++) [] (0, length grid - 1) (wordsPairMap 0 [] wordsPair)
        wordsPairMap num acc [] = acc
        wordsPairMap num acc (x:xs) = wordsPairMap (num + 1) ((fst x, [num]) : acc) xs

-- puzzle implementation of Kakuro
instance Puzzle Kakuro where
  solved (Kakuro grid _ _ _) = all (/=0) grid

  -- choices delegates to choicesR to give scope to the masterGrid for use in
  -- validDigits later
  choices k@(Kakuro masterGrid _ _ _) = choicesR k
    where
     choicesR (Kakuro (g:gs) c words (l:ls))
     -- If g /= 0, that cell has already been guessed.  Recurse after pulling
     -- off the corresponding grid cell and links element.  Be sure to replace
     -- those on all the results of the recursion.
      | g /= 0    = map restore $ choicesR (Kakuro gs c words ls)
     -- If g == 0, we found a cell to guess on.  Comprehend a list of Kakuros
     -- replacing g with all 'possibilities'
      | otherwise = [Kakuro (guess:gs) c words (l:ls) | guess <- possibilities ]
          where
            restore (Kakuro gs c ws ls) = Kakuro (g:gs) c ws (l:ls)
            -- the possibilities are found by intersecting together lists of
            -- the validDigits of the Words linked to the current cell
            possibilities = foldr intersect [1..9] digitslist
            digitslist = map ((validDigits masterGrid) . (words!!)) l

-- demo list from the writeup, for reference included here            
demo = [
  across 1 1 16 2, down 1 1 23 3,
  across 1 4 12 2, down 1 6 16 2,
  across 2 1 17 5, down 2 1 41 7,
  across 3 1 20 4, down 3 2 32 5,
  across 4 2 24 3, down 4 1 29 7,
  across 5 2 29 4, down 5 1 13 2,
  across 6 1 35 5, down 5 5 17 3,
  across 7 1 15 2,
  across 7 4  3 2 ] 
