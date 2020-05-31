module Main where

import Control.Monad ((>=>), forever, guard)
import Graphics.Gloss.Data.Vector (magV) -- Grab the magnitude function.

import Graphics.WorldTurtle

main :: IO ()
main = runWorld $ 
  makeTurtle' (0, 0) north red >>= forever . (drawStar >=> guard)

drawStar :: Turtle -> WorldCommand Bool
drawStar = run $ do
  -- Draw a star point.
  forward 200
  left 170
  -- If the magnitude of the position is <= 1 then return True.
  (1 <= ) . magV <$> position
