{- This logic for this sample is attributed to this excellent Go example:
   https://goplay.space/#61SJKVrWwj,8
-}
module Main where

import Control.Monad (when)

import WorldTurtle

branch :: Float -> Float -> Float -> Int -> Turtle -> TurtleCommand ()
branch size ratio angle iterations t = do
  setPenDown True t
  setPenSize size t
  forward (size * 20) t
  setPenDown False t
 
  when (iterations > 0) $ do
    left angle t
    branch (size * ratio) ratio angle (iterations - 1) t

    right (angle * 2) t
    branch (size*ratio) ratio angle (iterations - 1) t

    left angle t

  left 180 t
  forward (size * 20) t
  left 180 t

main :: IO ()
main = runTurtle $ makeTurtle >>= branch 5 0.7 30 5
