-- This logic for this sample is attributed to this excellent Go example:
-- https://goplay.space/#61SJKVrWwj,8
module Main where

import Control.Monad (when)

import Graphics.WorldTurtle

tree :: Float -> Float -> Float -> Int -> TurtleCommand ()
tree size ratio angle iterations = do
  setPenDown True
  setPenSize size
  forward s20
  setPenDown False
 
  when (iterations > 0) $ do
    left angle
    tree sr ratio angle (iterations - 1)

    right (angle * 2)
    tree sr ratio angle (iterations - 1)

    left angle

  left 180
  forward s20
  left 180
  where s20 = size * 20
        sr  = size * ratio

main :: IO ()
main = runTurtle $ setRotationSpeed 50 >> tree 5 0.7 30 5
