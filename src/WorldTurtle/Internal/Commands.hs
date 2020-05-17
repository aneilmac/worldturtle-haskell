{-# LANGUAGE TemplateHaskell #-}
module WorldTurtle.Internal.Commands
  ( Turtle 
  , TurtleCommand
  , TSC (..)
  , renderTurtle
  , addPicture
  , simTime
  , setSimTime
  , decrementSimTime
  , pics
  , exitCall
  , totalSimTime
  , turtles
  , makeTurtle
  , turtleData
  , updateTurtle
  ) where

import WorldTurtle.Internal.Turtle

import Graphics.Gloss.Data.Picture (Picture ,pictures)

import Control.Monad.Cont
import Control.Monad.State
import Control.Lens.TH

import Data.Map (Map)
import qualified Data.Map as Map

type TurtleState = State TSC

type TurtleCommand a = ContT () TurtleState a

newtype Turtle = Turtle Int deriving (Eq, Ord)

data TSC = TSC 
  { _pics :: [Picture]
  , _exitCall :: TurtleCommand ()
  , _totalSimTime :: Float
  , _turtles :: Map Turtle TurtleData 
  , _nextTurtleId :: Int
  }

defaultTSC :: Float -> TSC
defaultTSC simTime = TSC 
           { _pics = []
           , _totalSimTime = simTime
           , _exitCall = return ()
           , _turtles = Map.empty
           , _nextTurtleId = 0
           }

$(makeLenses ''TSC)

-- | Gets the remaining simulation time of the current turtle process.
simTime :: TurtleCommand Float
simTime = _totalSimTime <$> lift get

setSimTime :: Float -> TurtleCommand ()
setSimTime newTime = do
  let newTime' = max 0 newTime
  ex <- lift $ get >>= 
    \s -> put s { _totalSimTime = newTime' } >> return (_exitCall s)
  when (newTime' <= 0) ex

decrementSimTime :: Float -> TurtleCommand ()
decrementSimTime duration = simTime >>= \t -> setSimTime (t - duration)

addPicture :: Picture -> TurtleCommand ()
addPicture p = lift $ get >>= \s -> put s { _pics = p : _pics s }

processTurtle :: TurtleCommand () -> Float -> TSC
processTurtle commands simTime  = 
  let drawS = runContT (callCC $ \exit -> (setupM commands $ exit ())) return
      setupM :: TurtleCommand () -> TurtleCommand () -> TurtleCommand ()
      setupM commands exit = do
        lift $ get >>= \s -> put s { _exitCall = exit }
        decrementSimTime 0 -- test for early exit and no draw 
        commands
   in execState drawS (defaultTSC simTime)

renderTurtle :: TurtleCommand () -> Float ->  Picture
renderTurtle c f = let s = processTurtle c f
                    in pictures $ _pics s ++ drawTurtles (_turtles s)

turtleData :: Turtle -> TurtleCommand TurtleData
turtleData t = get >>= \s -> return $ (Map.!) (_turtles s) t

drawTurtles :: Map Turtle TurtleData -> [Picture]
drawTurtles m = fmap drawTurtle $ Map.elems m 

makeTurtle :: TurtleCommand Turtle
makeTurtle = do
  s <- get
  let i = _nextTurtleId s
  let t = Turtle i
  s' <- updateTurtle t defaultTurtle
  put s' { _nextTurtleId = i + 1 }
  return t

updateTurtle :: Turtle -> TurtleData -> TurtleCommand TSC
updateTurtle t d = do
  s <- get
  let m = Map.insert t d $ _turtles s
  put s { _turtles = m }
  get