{-# LANGUAGE FlexibleContexts,
             TemplateHaskell,
             TypeOperators,
             BangPatterns,
             QuasiQuotes #-}
module Game.Life.Core where

import Control.Monad.Identity (runIdentity)
import qualified Data.Array.Repa as R (map, zipWith)
import Data.Array.Repa
import Data.Array.Repa.Stencil
import Data.Array.Repa.Stencil.Dim2
import Data.Array.Repa.Algorithms.Randomish as RA
import System.Random

type Coord = (Int, Int)
type Coords = [Coord]
type Grid = Array U DIM2 Int
type GridI = (Grid, Coords)

printGrid :: Grid -> IO ()
printGrid !g = putStrLn $ showGrid g

showGrid :: Grid -> String
showGrid !g = show g
    where
        (width, height) = gridDim g

gridDim :: Grid -> Coord
gridDim !g = (width, height)
    where
        (Z :. width :. height) = extent g

gridList :: Grid -> [Int]
gridList !g = toList g

gridLength :: Grid -> Int
gridLength !g = height
    where
        (width, height) = gridDim g

gridCoords :: Grid -> Coords
gridCoords !g = [(x, y) | y <- [1..height], x <- [1..width]]
    where
        (width, height) = gridDim g

getCell :: Grid -> Coord -> Int
getCell !g !(x,y) = g ! (Z :. x :. y)


randomGrid :: Int -> IO Grid
randomGrid !x = do
            g <- newStdGen
            let randList = take (x^2) (randomRs (0,1) g :: [Int])
                in return $ fromListUnboxed (Z :. x :. x) randList


sten :: Stencil DIM2 Int
sten = [stencil2|  1 1 1
                   1 0 1
                   1 1 1 |]

evolveGrid :: Grid -> Grid
evolveGrid !grid = runIdentity . computeP $ R.zipWith evolveCell grid neighbors
    where
        neighbors = mapStencil2 (BoundConst 0) sten grid

{-# INLINE evolveCell #-}
evolveCell :: Int -> Int -> Int
evolveCell !1 !2 = 1
evolveCell !1 !3 = 1
evolveCell !1 !_ = 0
evolveCell !0 !3 = 1
evolveCell !0 !_ = 0
