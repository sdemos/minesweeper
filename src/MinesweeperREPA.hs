module MinesweeperREPA where

import qualified Data.Array.Repa as R
import           Data.Array.Repa.Stencil
import           Data.Array.Repa.Algorithms.Randomish

intersperse :: a -> Vector a -> Vector a
intersperse s v | null v    = v
                | otherwise = head v `cons` prependToAll s (tail v)

prependToAll :: a -> Vector a -> Vector a
prependToAll s v | null v    = v
                 | otherwise = s `cons` (head v `cons` prependToAll s (tail v))

intercalate :: [a] -> Vector [a] -> [a]
intercalate = (foldr1 (Prelude.++) .) . intersperse

twodupdate :: Vector (Vector a) -> (Int,Int) -> a -> Vector (Vector a)
twodupdate vs (x,y) a = vs // [( x, (vs!x) // [(y,a)] )]

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
data Field = Field { getWidth  :: Int
                   , getHeight :: Int
                   , getField  :: R.Array R.U R.DIM2 Square
                   }

instance Show Danger where
    show Mine       = "*"
    -- any danger level is guarenteed to be 1 digit, because the most neighbors
    -- any square can have is 8
    show (Danger d) = show d

instance Show Square where
    show (Square d True)  = show d
    show (Square _ False) = " "
    --debug show
    --show (Square d _) = show d

instance Show Field where
    show = intercalate "\n" . map (intercalate " " . map show) . toUnboxed . getField

-- generate a new field using one rng for x and one rng for y
generateField :: (RandomGen g)
              => g   -- random number generator for x
              -> g   -- random number generator for y
              -> Int -- height of minefield
              -> Int -- width of minefield
              -> Int -- Number of mines
              -> Field
generateField gx gy x y n = 
    where rx = randomRs (0,x-1) gx
          ry = randomRs (0,y-1) gy
          ur = takeUnique n (zip rx ry) []
          f  = Field x y
