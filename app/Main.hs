module Main where

import WorldTurtle
import qualified Graphics.Gloss.Data.Picture as G
import Control.Monad (forM_, mapM, replicateM_)
import Control.Applicative (empty, (<|>))

box :: TurtleCommand ()
box = do
  t1 <- makeTurtle' (10, 0)  0 green
  t2 <- makeTurtle' (0, 0) 90 red
  forward t1 90 <|> forward t2 90

box' :: TurtleCommand ()
box' = do
  t <- makeTurtle' (10, 0)  0 green

  replicateM_ 4 $ do
    fd t 90
    lt t 90

loop :: Turtle -> TurtleCommand() 
loop t = do 
  setSpeed t 300
  replicateM_ 4 $ do
    circle t 90 90
    stamp t
  left t 5

dragonSequence :: TurtleCommand ()
dragonSequence = do
  -- Generate our turtles
  let make = makeTurtle' (0, 0)
  t1 <- make 0  black
  t2 <- make 90 blue
  t3 <- make 180 red
  t4 <- make 270 green

  -- Run this animation on our turtles
  replicateM_ 18 $  loop t1
                <|> loop t2
                <|> loop t3
                <|> loop t4

circ :: TurtleCommand ()
circ = do
  t0 <- makeTurtle
  goto t0 (10, 10)
  t <- makeTurtle
  circle t 90 360

main :: IO ()
main = runTurtle dragonSequence -- box' -- dragonSequence

{-
main :: IO ()
main = runTurtle $ do
  t1 <- makeTurtle
  t2 <- makeTurtle 
  right t1 90
  forward t1 90 <|> forward t2 45
  right t2 90
  forward t2 90
-}