module Main where

import Control.Monad (replicateM_)

import WorldTurtle

main :: IO ()
main = runTurtle $ do
  -- Generate our turtles
  t1 <- makeT 0  black
  t2 <- makeT 90 blue
  t3 <- makeT 180 red
  t4 <- makeT 270 green

  -- Run this animation on our turtles
  replicateM_ 18 $  loop t1 <|> loop t2 <|> loop t3 <|> loop t4

  where makeT r c = do -- Helper function for generating turtles.
          t <- makeTurtle' (0, 0) r c
          setSpeed 300 t
          return t

-- Tells the turtle to make a loop then turn 5 degrees to the left!
loop :: Turtle -> TurtleCommand() 
loop t = do 
  circle 90 360 t
  left 5 t