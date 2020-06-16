{-# LANGUAGE BangPatterns #-}
module Main where

import Control.Lens ((^.))
import Control.Monad(replicateM_)

import Graphics.WorldTurtle
import Graphics.WorldTurtle.Internal.Commands
import Graphics.WorldTurtle.Internal.Sequence

import System.Environment (getArgs)

parallelCircles :: WorldCommand ()
parallelCircles =  do
  -- Generate our turtles
  t1 <- makeTurtle
  t2 <- makeTurtle
  replicateM_ 10000 $ do
    t1 >/> circle 90 <|> t2 >/> circle (-90)

main :: IO ()
main = do
  [t] <- getArgs
  (_, b) <- return $! processTurtle (seqW parallelCircles) (defaultTSC $ read t)
  mapM_ (putStrLn . show) (b ^. pics)
  return ()
