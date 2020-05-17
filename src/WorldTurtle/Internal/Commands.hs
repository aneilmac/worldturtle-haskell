{-# LANGUAGE TemplateHaskell #-}
module WorldTurtle.Internal.Commands
  ( Turtle 
  , SequenceCommand
  , TSC (..)
  , AlmostVal
  , renderTurtle
  , addPicture
  , simTime
  , setSimTime
  , decrementSimTime
  , pics
  , exitCall
  , totalSimTime
  , turtles
  , generateTurtle
  , turtleData
  , updateTurtle
  , animate'
  , animate
  , combineSequence
  ) where

import WorldTurtle.Internal.Turtle

import Graphics.Gloss.Data.Picture (Picture, pictures)

import Data.Semigroup ((<>))
import Control.Monad.Cont
import Control.Monad.State
import Control.Lens.TH

import Data.Either (Either(..), isLeft)
import Data.Void
import Data.Map (Map)
import qualified Data.Map as Map

type AlmostVal a = Either () a

type TurtleState b = State (TSC b)

type SequenceCommand b a = ContT b (TurtleState b) a

newtype Turtle = Turtle Int deriving (Eq, Ord)

data TSC b = TSC
  { _pics :: [Picture]
  , _exitCall :: SequenceCommand b b
  , _totalSimTime :: Float
  , _turtles :: Map Turtle TurtleData 
  , _nextTurtleId :: Int
  }

defaultTSC :: Float -> TSC b
defaultTSC simTime = TSC 
           { _pics = []
           , _totalSimTime = simTime
           , _exitCall = undefined
           , _turtles = Map.empty
           , _nextTurtleId = 0
           }

$(makeLenses ''TSC)

-- | Gets the remaining simulation time of the current turtle process.
simTime :: SequenceCommand b Float
simTime = _totalSimTime <$> lift get

setSimTime :: Float -> SequenceCommand b ()
setSimTime newTime = do
  let newTime' = max 0 newTime
  ex <- lift $ get >>= 
    \s -> put s { _totalSimTime = newTime' } >> return (_exitCall s)
  when (newTime' <= 0) $ void ex

decrementSimTime :: Float -> SequenceCommand b ()
decrementSimTime duration = simTime >>= \t -> setSimTime (t - duration)

addPicture :: Picture -> SequenceCommand b ()
addPicture p = lift $ get >>= \s -> put s { _pics = p : _pics s }

setupM :: SequenceCommand (AlmostVal a) a 
       -> (SequenceCommand (AlmostVal a) (AlmostVal a))
       -> SequenceCommand (AlmostVal a) (AlmostVal a)
setupM commands exit = do
  lift $ get >>= \s -> put s { _exitCall = exit }
  Right <$> commands

exitCondition :: SequenceCommand (AlmostVal a) a
              -> SequenceCommand (AlmostVal a) (AlmostVal a)
exitCondition commands = callCC $ \exit -> setupM commands (exit $ Left ())

processTurtle :: SequenceCommand (AlmostVal a) a 
              -> TSC (AlmostVal a)
              -> (AlmostVal a, TSC (AlmostVal a))
processTurtle commands tsc = 
  let drawS = runContT (exitCondition commands) return
   in runState drawS tsc

renderTurtle :: SequenceCommand (AlmostVal a) a -> Float ->  Picture
renderTurtle c f = let (_, s) = processTurtle c (defaultTSC f)
                    in pictures $ _pics s ++ drawTurtles (_turtles s)

turtleData :: Turtle -> SequenceCommand b TurtleData
turtleData t = get >>= \s -> return $ (Map.!) (_turtles s) t

drawTurtles :: Map Turtle TurtleData -> [Picture]
drawTurtles m = fmap drawTurtle $ Map.elems m 

generateTurtle :: SequenceCommand b Turtle
generateTurtle = do
  s <- get
  let i = _nextTurtleId s
  let t = Turtle i
  updateTurtle t defaultTurtle
  s' <- get -- updated state
  put s' { _nextTurtleId = i + 1 }
  return t

updateTurtle :: Turtle -> TurtleData -> SequenceCommand b ()
updateTurtle t d = do
  s <- get
  let m = Map.insert t d $ _turtles s
  put s { _turtles = m }

animate' :: Float 
         -> Float 
         -> (Float -> SequenceCommand b a) 
         -> SequenceCommand b a
animate' distance speed callback =
   let duration = distance / speed
       d' = if isNaN duration || isInfinite duration then 0 else duration
       -- ^ if speed is 0 we use this as a "no animation" command from 
       --   user-space.
     in animate (abs d') callback

animate :: Float -> (Float -> SequenceCommand b a) -> SequenceCommand b a
animate duration callback = do
   timeRemaining <- simTime -- simulation time to go
   let availableTime = min timeRemaining duration
   -- ^ Amount of time we have to complete the animation before we need to exit.
   let timeQuotient = if availableTime == 0 then 1 else availableTime / duration
   -- ^ quotient of available time vs required time. Note that when the duration
   --   is 0 we say "don't do any animation"
   t <- callback timeQuotient 
   -- ^ Perform the calculation with the quotient for lerping
   decrementSimTime availableTime 
   -- ^ Test to see if this is the end of our animation and if so exit early
   return t

combineSequence :: Semigroup a
                => SequenceCommand (AlmostVal b) a
                -> SequenceCommand (AlmostVal b) a
                -> SequenceCommand (AlmostVal b) a
combineSequence a b = do
  s <- get

  aVal <- callCC $ \ exitFromA -> do
    put s { _exitCall = exitFromA (Left ())
          , _pics = []
          }
    Right <$> a
  s' <- get
  
  bVal <- callCC $ \ exitFromB -> do
    put s' { _exitCall = exitFromB (Left ())
          , _pics = []
          , _totalSimTime = _totalSimTime s
          }
    Right <$> b
  s'' <- get
  
  let elapsedATime = _totalSimTime s - _totalSimTime s'
  let elapsedBTime = _totalSimTime s - _totalSimTime s''
  let mostTime = max elapsedATime elapsedBTime

  put s'' { _pics = _pics s ++ _pics s' ++ _pics s''
          , _exitCall =  _exitCall s
          , _totalSimTime = mostTime
          }

  when (isLeft aVal || isLeft bVal) $ do
    void $ _exitCall <$> get

  let (Right aVal') = aVal
  let (Right bVal') = bVal
  return $ aVal' <> bVal'
  