module Main where

import Control.Monad (replicateM_) -- Required control flow functions.

import Graphics.WorldTurtle

main :: IO ()
main = runTurtle $ replicateM_ 4 $ forward 90 >> right 90
