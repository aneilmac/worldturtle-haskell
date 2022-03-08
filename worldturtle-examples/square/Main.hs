module Main where

import Graphics.WorldTurtle

main :: IO ()
main = runTurtle $ repeatFor 4 $ forward 90 >> right 90
