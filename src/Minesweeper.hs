{-# LANGUAGE QuasiQuotes #-}

module Minesweeper where

import Prelude hiding (map, zipWith)

import           Data.Maybe (fromJust)

import qualified Data.Vector as V
import           Data.Array.Repa as R hiding ((++))
import           Data.Array.Repa.Repr.Vector
import           Data.Array.Repa.Stencil
import           Data.Array.Repa.Stencil.Dim2
import           Data.Array.Repa.Algorithms.Randomish

import System.Random

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

emptySquare = Square (Danger 0) False

-- For minesweeper, there are a couple of things that we need to keep track of
-- First, we need the locations of the mines
-- We also need which squares are revealed at any given time
-- the numbers are calculatable, but I think that we should calculate them
-- once at the beginning and then store them
-- the field is row-major
-- note: with the new repa representation, the width and height of the array
-- are encoded in the array as the shape
data Field = Field { getField :: Array V DIM2 Square }

instance Show Danger where
    show Mine       = "*"
    -- any danger level is guarenteed to be 1 digit, because the most neighbors
    -- any square can have is 8
    --show (Danger 0) = " "
    show (Danger d) = show d

instance Show Square where
    show (Square d True)  = show d
    show (Square _ False) = " "
    --debug show
    --show (Square d _) = show d

instance Show Field where
    show (Field f) = let (Z :. x :. y) = extent f in
        V.foldl1' (++)
          $ V.imap (\i a -> if i /= 0 then (if mod i y == 0 then '\n' else ' ') : show a else show a)
          $ toVector f

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
generateField g x y n = updateDangers $ Field f
    where rs = takeUnique n $ randomRs (0, (x * y) - 1) g
          fv = V.replicate (x * y) emptySquare V.// zip rs (repeat $ Square Mine False)
          f  = fromVector (Z :. x :. y) fv

updateDangers :: Field -> Field
updateDangers (Field f) = Field
                        . computeVectorS -- convert from delayed array back to vector array
                        . zipWith updateDanger f -- update the field with the new dangers we found
                        . mapStencil2 (BoundConst 0) stencil -- perform the convolution
                        . map (\s->if isMine s then 1 else 0) -- turn mines into 1s and others into 0s
                        $ f
    where stencil = [stencil2|1 1 1
                              1 0 1
                              1 1 1|]

updateDanger :: Square -> Int -> Square
updateDanger (Square (Danger _) r) d = Square (Danger d) r
updateDanger s _ = s

isMine :: Square -> Bool
isMine (Square Mine _) = True
isMine _               = False

isRevealedMine :: Square -> Bool
isRevealedMine (Square Mine True) = True
isRevealedMine _                  = False

isMineOrRevealed :: Square -> Bool
isMineOrRevealed (Square Mine False)      = True
isMineOrRevealed (Square (Danger _) True) = True
isMineOrRevealed _                        = False

-- reveal spot
-- coordinates are measured from top left of grid
-- the basic reveal is to just set the reveal mask for that spot to be True,
-- and return the Left Field if that spot is a mine, and Right Field if it's
-- not
-- past that, you could reasonably space-fill the revealed area if there are
-- no mines in the surrounding areas, much like most implementations for
-- playing but I think we can leave that task on the consumer of the game,
-- and just have this a "recorder of events" of sorts
reveal :: Int   -- x value of spot to reveal
       -> Int   -- y value of spot to reveal
       -> Field -- field to reveal spot on
       -> Field -- no computation is done to imply whether or not a mine was hit
reveal x y (Field f) = Field (fromJust (computeP (R.traverse f id (update x y))))
  where update x y square s@(Z :. sx :. sy) | x == sx && y == sy = (square s){isRevealed = True}
        update x y square s = square s

hitMine :: Field -> Bool
hitMine = fromJust . foldAllP (||) False . R.map isRevealedMine . getField

solved :: Field -> Bool
solved = fromJust . foldAllP (&&) True . R.map isMineOrRevealed . getField
