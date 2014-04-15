module Main where

import Game.Life

main :: IO ()
main = randomGrid 100 >>= guiSession "John Conway's Game of Life"
