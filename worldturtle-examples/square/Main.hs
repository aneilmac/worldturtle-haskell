module Main where

import Control.Monad (replicateM_) -- Required control flow functions.

import WorldTurtle

main :: IO ()
main = runTurtle $ do
  t <- makeTurtle
  replicateM_ 4 $ do
    forward 90 t
    right 90 t