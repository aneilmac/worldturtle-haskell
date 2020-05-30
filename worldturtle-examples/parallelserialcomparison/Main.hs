module Main where

import Control.Monad (replicateM_) -- Required control flow functions.

import Graphics.WorldTurtle

drawSquare :: Turtle -> WorldCommand ()
drawSquare = run $ replicateM_ 4 $ forward 90 >> right 90

makeTurtles :: Point -> WorldCommand (Turtle, Turtle)
makeTurtles p =  do
  t1 <- makeTurtle' p 0 green
  t1 >/> left 45

  t2 <- makeTurtle' p 0 red
  t2 >/> right 135

  return (t1, t2)

main :: IO ()
main = runWorld $ do
  (t1, t2) <- makeTurtles (0, 70)
  (t3, t4) <- makeTurtles (0, -70)
  (drawSquare t1 >> drawSquare t2) <|> (drawSquare t3 <|> drawSquare t4)