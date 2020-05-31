-- Credit for this implementation goes to:
-- https://juliagraphics.github.io/Luxor.jl/stable/turtle/
module Main where

import Control.Monad (forM_)

import Graphics.WorldTurtle

forwards :: [Float]
forwards = take 400 [1, 1.75..]

spiral :: TurtleCommand ()
spiral = do
  setPenColor cyan
  setPenSize 1.5
  setSpeed 500
  setRotationSpeed 0 -- instant turns
  forM_ forwards $ \ f -> do
    forward f
    left 89.5
    penColor >>= setPenColor . shiftHue 1

main :: IO ()
main = runTurtle spiral

