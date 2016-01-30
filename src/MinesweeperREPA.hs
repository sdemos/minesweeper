module MinesweeperREPA where

import Prelude hiding (intersperse, intercalate)

import qualified Data.Vector as V
import qualified Data.Array.Repa as R
import qualified Data.Array.Repa.Repr.Vector as R
import qualified Data.Array.Repa.Stencil as R
import qualified Data.Array.Repa.Algorithms.Randomish as R

import System.Random

intersperse :: a -> V.Vector a -> V.Vector a
intersperse s v | V.null v    = v
                | otherwise = V.head v `V.cons` prependToAll s (V.tail v)

prependToAll :: a -> V.Vector a -> V.Vector a
prependToAll s v | V.null v    = v
                 | otherwise = s `V.cons` (V.head v `V.cons` prependToAll s (V.tail v))

intercalate :: [a] -> V.Vector [a] -> [a]
intercalate = (V.foldr1 (Prelude.++) .) . intersperse

-- the danger rating of a minesweeper square
-- I think that the Danger type is isomorphic to Maybe Integer
-- if it helps me to have a monad/functor/applicative instance
-- for danger, I can just use Maybe's implementation and it should
-- work.
data Danger = Mine | Danger Int deriving Eq

-- a square keeps track of if it is revealed, as well as the danger rating
-- of the location it is in
data Square = Square { getDanger  :: Danger
                     , isRevealed :: Bool
                     } deriving Eq

-- For minesweeper, there are a couple of things that we need to keep track of
-- First, we need the locations of the mines
-- We also need which squares are revealed at any given time
-- the numbers are calculatable, but I think that we should calculate them 
-- once at the beginning and then store them
-- the field is row-major
-- note: with the new repa representation, the width and height of the array
-- are encoded in the array as the shape
data Field = Field { getField :: R.Array R.V R.DIM2 Square }

instance Show Danger where
    show Mine       = "*"
    -- any danger level is guarenteed to be 1 digit, because the most neighbors
    -- any square can have is 8
    show (Danger d) = show d

instance Show Square where
    --show (Square d True)  = show d
    --show (Square _ False) = " "
    --debug show
    show (Square d _) = show d

instance Show Field where
    show (Field f) = let (R.Z R.:. x R.:. y) = R.extent f in 
        V.foldl1' (++)
          $ V.imap (\i a -> if i /= 0 then (if mod i y == 0 then '\n' else ' ') : show a else show a)
          $ R.toVector f

-- take n unique values from a list. the third argument is the seen list
takeUnique :: (Eq n, Eq a, Num n) => n -> [a] -> [a]
takeUnique = takeUnique' []
takeUnique' :: (Eq n, Eq a, Num n) => [a] -> n -> [a] -> [a]
takeUnique' seen 0 _      = seen
takeUnique' seen n (x:xs) | x `notElem` seen = takeUnique' (x:seen) (n-1) xs
                          | otherwise        = takeUnique' seen n xs

-- generate a new field using one rng for x and one rng for y
generateField :: (RandomGen g)
              => g   -- random number generator
              -> Int -- height of minefield
              -> Int -- width of minefield
              -> Int -- Number of mines
              -> Field
generateField g x y n = Field f
    where rs = takeUnique n $ randomRs (0, (x * y) - 1) g
          fv = V.replicate (x * y) (Square (Danger 0) False) V.// zip rs (repeat $ Square Mine False)
          f  = R.fromVector (R.Z R.:. x R.:. y) fv
