module Minesweeper where

import Prelude hiding (map, zipWith, intersperse, null, head, tail, foldr1, replicate)
import Data.Vector
import System.Random

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
                   , getField  :: Vector (Vector Square) }

instance Show Danger where
    show Mine       = "*"
    -- any danger level is guarenteed to be 1 digit, because the most neighbors
    -- any square can have is 9
    show (Danger d) = show d

instance Show Square where
    show (Square d True)  = show d
    show (Square _ False) = " "
    --debug show
    --show (Square d _) = show d

instance Show Field where
    show = intercalate "\n" . map (intercalate " " . map show) . getField

-- generate a new field using one rng for x and one rng for y
generateField :: (RandomGen g)
              => g   -- random number generator for x
              -> g   -- random number generator for y
              -> Int -- height of minefield
              -> Int -- width of minefield
              -> Int -- Number of mines
              -> Field
generateField gx gy x y n = field
    where rx = randomRs (0,x-1) gx
          ry = randomRs (0,y-1) gy
          ur = takeUnique n (Prelude.zip rx ry) []
          f  = Field x y $ replicate x (replicate y (Square (Danger 0) False))
          rf = Prelude.foldr (\c a->Field x y $ twodupdate (getField a) c (Square (Mine) False)) f ur
          -- calculate dangers for non-mine squares
          field = updateDangers rf

-- this whole section about updating the dangers for a map of mines can be implemented really easily
-- with repa stencils.
updateDangers :: Field -> Field
updateDangers f = f { getField = imap (\i->imap (\j c->updateDanger f i j)) $ getField f }

updateDanger :: Field -> Int -> Int -> Square
updateDanger f r c | hasMine f r c = (getField f) ! r ! c
updateDanger f r c = ((getField f) ! r ! c) { getDanger = Danger danger }
    where y = if nt || nl then 0 else ( if hasMine f (r-1) (c-1) then 1 else 0)
          k = if nt       then 0 else ( if hasMine f (r-1) (c  ) then 1 else 0)
          u = if nt || nr then 0 else ( if hasMine f (r-1) (c+1) then 1 else 0)
          h = if nl       then 0 else ( if hasMine f (r  ) (c-1) then 1 else 0)
          l = if nr       then 0 else ( if hasMine f (r  ) (c+1) then 1 else 0)
          b = if nb || nl then 0 else ( if hasMine f (r+1) (c-1) then 1 else 0)
          j = if nb       then 0 else ( if hasMine f (r+1) (c  ) then 1 else 0)
          n = if nb || nr then 0 else ( if hasMine f (r+1) (c+1) then 1 else 0)
          nt = r == 0
          nb = r == getHeight f - 1
          nl = c == 0
          nr = c == getWidth f - 1
          danger = y+u+h+j+k+l+b+n

hasMine :: Field -> Int -> Int -> Bool
hasMine f x y = isMine ((getField f)!x!y)

isMine :: Square -> Bool
isMine (Square Mine _) = True
isMine _               = False

-- generate a new field by creating two new rngs and passing them to generateField
newField :: Int -> Int -> Int -> IO Field
newField x y n = do
    gx <- newStdGen
    gy <- getStdGen
    return $ generateField gx gy x y n

-- take n unique values from a list. the third argument is the seen list
takeUnique :: (Eq n, Eq a, Num n) => n -> [a] -> [a] -> [a]
takeUnique 0 _      seen = seen
takeUnique n (x:xs) seen | x `Prelude.notElem` seen = takeUnique (n-1) xs (x:seen)
                         | otherwise                = takeUnique n xs seen

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
       -> Either Field Field -- left implies you hit a mine
reveal x y f = if getDanger square == Mine then Left rfield else Right rfield
    where square = (getField f) ! x ! y
          rfield = f { getField = twodupdate (getField f) (x,y) $ square { isRevealed = True } }
