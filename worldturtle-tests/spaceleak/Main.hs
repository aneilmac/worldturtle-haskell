module Main where

import Control.Monad

import qualified Graphics.Gloss.Interface.Pure.Display as G

import Graphics.WorldTurtle
import Graphics.WorldTurtle.Internal.Commands
import Graphics.WorldTurtle.Internal.Sequence

import System.Environment (getArgs)

parallelCircles :: WorldCommand ()
parallelCircles =  do
  -- Generate our turtles
  t1 <- makeT 0  black
  t2 <- makeT 90 blue
  t3 <- makeT 180 red
  t4 <- makeT 270 green

  -- Run this animation on our turtles
  replicateM_ 18 $  loop t1 <|> loop t2 <|> loop t3 <|> loop t4

  where makeT r c = do -- Helper function for generating turtles.
          t <- makeTurtle' (0, 0) r c
          run t $ setSpeed 300
          return t
        loop t = run t $ circle 90 >> left 5

main :: IO ()
main = do
  [t] <- getArgs
  G.display G.FullScreen white $ renderTurtle (seqW parallelCircles) (read t)
