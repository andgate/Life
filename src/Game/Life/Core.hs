module Game.Life.Core where

import Control.Concurrent
import Data.Ix
import Data.List.Split
import System.Random
import qualified Data.Array.Repa as R
import Data.Array.Repa ((:.) (..))

type Coord = (Int, Int)
type Coords = [Coord]
type Cell = Bool
--type CellRef = IORef Cell
type Board = [[Cell]]
type Grid = R.Array R.U R.DIM2 Bool


liveCell, deadCell :: Cell
liveCell = True
deadCell = False

-- empty board of a given dimension
emptyBoard :: Int -> Board
emptyBoard n = replicate n deadRow
    where deadRow = replicate n deadCell

-- random board funtimes, funtimes...
randBoard :: Int -> IO Board
randBoard x = do
            g <- newStdGen
            let randList = take (x^2) (randoms g :: [Bool])
                in return $ chunksOf x randList

-- random grid fun fun fun looking forward to the weekend
randGrid :: Int -> IO Grid
randGrid x = do
            g <- newStdGen
            let randList = take (x^2) (randoms g :: [Bool])
                in return $ R.fromListUnboxed (R.Z :. x :. x) randList


showCell :: Cell -> Char
showCell True = '#'
showCell False = '.'

showBoard :: Board -> String
showBoard board = concat [(map showCell row) ++ "\n" | row <- board]

loadBoard :: String -> IO Board
loadBoard [] = error "No file given"
loadBoard file = do contents <- readFile file
                    let b = readBoard contents
                        in verifyBoard b

readBoard :: String -> Board
readBoard s = [map readCell line | line <- (lines s)]

verifyBoard :: Board -> IO Board
verifyBoard b = if isUniform b
                    then return b
                    else error "Non-uniform board"

isUniform :: Board -> Bool
isUniform (l:[]) = True
isUniform (l1:l2:ls) = (length l1) == (length l2) && isUniform (l2:ls)

readCell :: Char -> Cell
readCell '#' = True
readCell '.' = False
readCell _ = False

getDim :: Board -> (Int, Int)
getDim b = (width, height)
    where    
        width = length (b!!0)
        height = length b

gridDim :: Grid -> (Int, Int)
gridDim grid = (width, height)
    where    
        (R.Z :. width :. height) = R.extent grid

boundsCheck :: Board -> (Int, Int) -> Bool
boundsCheck b (x,y) = (inRange (1, width) x) && (inRange (1, height) y)
    where
        (width, height) = getDim b

-- Get cell from board or a deadCell.
getCell :: Board -> (Int, Int) -> Cell
getCell b (x, y)
    | boundsCheck b (x,y) = cell
    | otherwise           = deadCell
    where
        cell = row!!(x-1)
        row = b!!(y-1)

-- List of cells surrounding the cell at (x,y)
neighborhood :: Board -> Coord -> [Cell]
neighborhood b xy
    | boundsCheck b xy = map (getCell b) (neighborCoords xy)

census :: [Cell] -> Int
census cs = length $ filter id cs

neighborCoords :: Coord -> Coords
neighborCoords (x, y) = filter (\(xi, yi) -> not (x == xi && y == yi))
                            $ regionCoords (x-1, y-1) (x+1, y+1)

boardCoords :: Board -> [Coord]
boardCoords b = regionCoords (1,1) (getDim b)

regionCoords :: Coord -> Coord -> Coords
regionCoords (x1, y1) (x2, y2)
        = [ (xi, yi) |
              yi <- [y1..y2],
              xi <- [x1..x2] ]

evolveCell :: Board -> Coord -> Cell
evolveCell b xy
    | n == 3 = liveCell
    | n == 2 = thisCell
    | otherwise = deadCell
    where
        thisCell = getCell b xy
        n = census $ neighborhood b xy


evolve :: Board -> Board
evolve b = {-# SCC "evolve" #-} chunksOf width cells
    where
        (width, _) = getDim b
        cells = map (evolveCell b) (boardCoords b)
