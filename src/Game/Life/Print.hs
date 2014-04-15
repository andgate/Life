module Game.Life.Print where

import Game.Life.Core

simulate :: Board -> IO ()
simulate b = let b1 = evolve b
                 in do
                    printBoard $ evolve b1
                    getChar
                    if b == b1
                        then return ()
                        else simulate b1

printBoard :: Board -> IO ()
printBoard board = putStrLn $ showBoard board
