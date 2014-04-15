module Main where

import System.Environment
import Game.Life

main :: IO ()
main = getArgs >>= parseArgs

parseArgs :: [String] -> IO ()
parseArgs [] = guiSession "Conway's Game of Life - Empty Board" =<< randBoard 65
--parseArgs [] = simulate =<< randBoard 100
parseArgs (arg:args) = (loadBoard arg) >>= guiSession arg
