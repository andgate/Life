module Game.Life.GUI where

import Data.Monoid ((<>), mconcat, mempty)
import Game.Life.Core
import Graphics.Gloss.Interface.IO.Game

-- Initialize the screen and start the loop
guiSession :: String -> Grid -> IO ()
guiSession title grid = playIO
    (InWindow title (500, 500) (500, 500))
    black
    10
    (grid, (gridCoords grid))
    drawGrid
    handleInput
    stepGame


drawGrid :: GridI -> IO Picture
drawGrid (g, coords) = return cells
    where
        cells = mconcat $ map (drawCell g cellSize) coords
        cellSize = 500 / boardSize
        boardSize = fromIntegral $ fst $ gridDim g
        offset = (-500) / 2

drawCell :: Grid -> Float -> Coord -> Picture
drawCell g cellSize (x,y)
    | cell == 1 = translate cellX cellY cellPic
    | otherwise = blank
    where
        cellPic = color white (rectangleSolid cellSize cellSize)
        cellX = ((fromIntegral x) * cellSize) - 250
        cellY = ((fromIntegral y) * cellSize) - 250
        cell = getCell g (x-1,y-1)
                 

handleInput :: Event -> GridI -> IO GridI
handleInput (EventKey (SpecialKey KeyF5) Up _ _) (g0, coords) = do
    g1 <- randomGrid (gridLength g0)
    return (g1, coords)
handleInput _ (grid, coords) = return (grid, coords)

stepGame :: Float -> GridI -> IO GridI
stepGame dx (grid, coords) = return (evolveGrid grid, coords)
    
