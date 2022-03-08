module Main where

import Control.Monad
import Graphics.WorldTurtle

main :: IO ()
main = runWorld $ do
  -- Generate our turtles
  t1 <- makeT 0  black
  t2 <- makeT 90 blue
  t3 <- makeT 180 red
  t4 <- makeT 270 green

  -- Run this animation on our turtles
  replicateM_ 18 $ loop t1 >!> loop t2 >!> loop t3 >!> loop t4

  where makeT r c = do -- Helper function for generating turtles.
          t <- makeTurtle' (0, 0) r c
          t >/> setSpeed 300
          return t

-- Tells the turtle to make a loop then turn 5 degrees to the left!
loop :: Turtle -> WorldCommand() 
loop = run $ circle 90 >> left 5
