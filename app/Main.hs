module Main where

import WorldTurtle
import qualified Graphics.Gloss.Data.Picture as G
import Control.Monad (forM_, mapM)
import Control.Applicative (empty, (<|>))

loop :: Turtle -> Float -> TurtleCommand() 
loop t d = do 
  setSpeed t 200
  right t d
  circle t 90 360

bd :: (Color, Float) -> TurtleCommand Turtle
bd (c, d) = do
  t <- makeTurtle
  setPenColor t c
  setHeading t d
  return t

main :: IO ()
main = runTurtle $ do
  -- Generate our turtles
  [t1, t2, t3, t4] <- mapM bd
                      [ (black, 0)
                      , (blue, 90)
                      , (red, 180)
                      , (green, 270)
                      ]
  -- Run this animation on our turtles
  forM_ [0, 10 .. 40] $ 
    \ v -> do loop t1 v 
          <|> loop t2 v
          <|> loop t3 v
          <|> loop t4 v
