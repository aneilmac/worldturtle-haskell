-- This logic for this sample is attributed to this excellent Go example:
-- https://goplay.space/#61SJKVrWwj,8
module Main where

import Control.Monad (when)

import Graphics.WorldTurtle

tree :: Float -> Float -> Float -> Int -> Turtle -> TurtleCommand ()
tree size ratio angle iterations t = do
  setPenDown True t
  setPenSize size t
  forward s20 t
  setPenDown False t
 
  when (iterations > 0) $ do
    left angle t
    tree sr ratio angle (iterations - 1) t

    right (angle * 2) t
    tree sr ratio angle (iterations - 1) t

    left angle t

  left 180 t
  forward s20 t
  left 180 t
  where s20 = size * 20
        sr  = size * ratio

main :: IO ()
main = runTurtle $ do
  t <- makeTurtle 
  setRotationSpeed 50 t
  tree 5 0.7 30 5 t
