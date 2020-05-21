module Main where

import Control.Monad (replicateM_) -- Required control flow functions.

import Graphics.WorldTurtle

drawSquare :: Turtle -> TurtleCommand ()
drawSquare t = replicateM_ 4 $ forward 90 t >> right 90 t

makeTurtles :: Point -> TurtleCommand (Turtle, Turtle)
makeTurtles p =  do
  t1 <- makeTurtle' p 0 green
  left 45 t1

  t2 <- makeTurtle' p 0 red
  right 135 t2

  return (t1, t2)

main :: IO ()
main = runTurtle $ do
  (t1, t2) <- makeTurtles (0, 70)
  (t3, t4) <- makeTurtles (0, -70)

  (drawSquare t1 >> drawSquare t2) <|> (drawSquare t3 <|> drawSquare t4)