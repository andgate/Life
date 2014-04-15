module Game.Life.GUI where

import Data.Monoid ((<>), mconcat, mempty)
import Game.Life.Core
import Graphics.Gloss.Interface.IO.Game

-- Initialize the screen and start the loop
guiSession :: String -> Board -> IO ()
guiSession title board = playIO
    (InWindow title (500, 500) (500, 500))
    black
    10
    board
    drawBoard
    handleInput
    stepGame


drawBoard :: Board -> IO Picture
drawBoard b = return cells
    where
        cells = mconcat $ map (drawCell b cellSize) (boardCoords b)
        cellSize = 500 / boardSize
        boardSize = fromIntegral $ fst $ getDim b
        offset = (-500) / 2

drawCell :: Board -> Float -> Coord -> Picture
drawCell b cellSize (x,y)
    | cell == True = translate cellX cellY cellPic
    | otherwise = blank
    where
        cellPic = color white (thickCircle 0 cellSize)
        cellX = ((fromIntegral x) * cellSize) - 250
        cellY = ((fromIntegral y) * cellSize) - 250
        cell = getCell b (x,y)
                 

handleInput :: Event -> Board -> IO Board
handleInput _ board = return board

stepGame :: Float -> Board -> IO Board
stepGame dx board = return $ evolve board
    
