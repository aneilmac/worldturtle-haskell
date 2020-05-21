module Main where

import Control.Monad (forM_) -- Required control flow functions.

import Graphics.Gloss.Data.Color

import WorldTurtle

colors :: [Color]
colors = [rose, violet, azure, aquamarine, chartreuse, orange]

steps :: [(Float, Color)]
steps = zip [0..20] $ cycle colors

main :: IO ()
main = runTurtle $ do
  t <- makeTurtle
  forM_ steps $ \ (i, c) -> do
    setPenColor c t
    forward (i * 10) t
    right 144 t