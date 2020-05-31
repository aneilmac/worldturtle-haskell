module Main where

import Graphics.WorldTurtle

main :: IO ()
main = runWorld $ do
  t1 <- makeTurtle' (0, 0) north green
  t2 <- makeTurtle' (0, 0) north red

  -- Demonstrate serial 
  sleep 1
  t1 >/> circle 90 >> t2 >/> circle (-90)
  
  clear

  -- Demonstrate parallel
  sleep 1
  t1 >/> circle 90 <|> t2 >/> circle (-90)
