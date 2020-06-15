{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_HADDOCK hide #-}
module Graphics.WorldTurtle.Internal.Sequence
  ( Turtle 
  , TSC
  , SequenceCommand
  , renderTurtle
  , addPicture
  , simTime
  , setSimTime
  , decrementSimTime
  , pics
  , totalSimTime
  , turtles
  , generateTurtle
  , animate'
  , animate
  , combineSequence
  , alternateSequence
  , failSequence
  ) where

import Graphics.WorldTurtle.Internal.Turtle

import Graphics.Gloss.Data.Picture (Picture, pictures)

import Control.Monad (when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
import Control.Monad.Trans.State

import Control.Lens

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

-- | State Monad that takes our `TSC` type as its state object.
type TurtleState = State TSC

-- | Continuation Monad on top of the State Monad of form @SequenceCommand b a@.
--   /b/ is the final return type of the entire Monad sequence - this is what 
--   will be returned if/when we need to exit early from anywhere in a great big
--   sequence of steps. /a/ is the return type of the current step of the 
--   animation sequence. That is: what will be passed into the next step.
type SequenceCommand b a = ExceptT b TurtleState a

-- Careful of editing the Turtle comment below as it is public docs!
-- Really "Turtle" is just a handle to internal TurtleData. It is a key that
-- looks up TurtleData in a map. Since Turtle is exposed to the user-level we 
-- do not document it in this way however.

-- | The Turtle that is drawn on the canvas! Create a new turtle using 
-- `Graphics.WorldTurtle.Commands.makeTurtle`.
newtype Turtle = Turtle Int deriving (Eq, Ord)

data TSC = TSC
  { _pics :: ![Picture] -- ^ All pictures that make up the current canvas
  , _totalSimTime :: !Float -- ^ Remaining available for animating
  , _turtles :: Map Turtle TurtleData  -- Collection of all turtles.
  , _nextTurtleId :: !Int -- ^ ID of next turtle to be generated.
  }

$(makeLenses ''TSC)

-- | Generates default parameter arguments. The TSC returned by this value
-- must never be used for sequencing as the exitCall is undefined and will only
-- be defined in the setup stage of the animation process.
defaultTSC :: Float -> TSC
defaultTSC givenTime = TSC 
           { _pics = []
           , _totalSimTime = givenTime
           , _turtles = Map.empty
           , _nextTurtleId = 0
           }

-- | Gets the remaining simulation time of the current turtle process.
-- The simulation time dictates how much time is remaining for an animation,
-- and it will be reduced as the animations play in sequence. Once this value
-- hits 0 the exit command will be called and the monad will stop processing.
simTime :: SequenceCommand b Float
simTime = use totalSimTime

-- | Sets the simulation time in the state monad.
-- If the simulation time is <= 0 then this setter will immediately call the
-- exit function which will kill any further processing of the monad.
setSimTime :: b
           -> Float
           -> SequenceCommand b ()
setSimTime e newTime = do
  let newTime' = max 0 newTime
  totalSimTime .= newTime'
  when (newTime' <= 0) (failSequence e)

-- | Takes a value away form the current sim time and store the updated time.
-- See `setSimTime`.
decrementSimTime :: b -- ^ Value to throw if fails.
                 -> Float -- ^ Value to subtract from store simulation time.
                 -> SequenceCommand b ()
decrementSimTime e duration = simTime >>= \ t -> setSimTime e (t - duration)

-- | Given a picture, adds it to the picture list.
addPicture :: Picture -- ^ Picture to add to our animation
           -> SequenceCommand b ()
addPicture p = pics %= (p :)

-- | Given a sequence and a State, returns the result of the computation and the
--   final state of the computation. When the result is @Right@, then the 
--   computation completed, otherwise the computation ended early due to lack of
--   time available (partial animation).
processTurtle :: SequenceCommand b a 
              -> TSC
              -> (Either b a, TSC)
processTurtle commands tsc = 
  let drawS = runExceptT $ commands
   in runState drawS tsc

-- | Given a computation to run and an amount of time to run it in, renders the
--   final "picture".
renderTurtle :: Monoid b 
             => SequenceCommand b a 
             -> Float 
             -> Picture
renderTurtle c f = let (_, s) = processTurtle c' t
                       c' = decrementSimTime mempty 0 >> c
                       t  = defaultTSC f
                    in pictures $ s ^. pics ++ drawTurtles (s ^. turtles)

drawTurtles :: Map Turtle TurtleData 
            -> [Picture]
drawTurtles m = drawTurtle <$> Map.elems m 

generateTurtle :: SequenceCommand b Turtle
generateTurtle = do
  t <- Turtle <$> use nextTurtleId
  turtles %= Map.insert t defaultTurtle
  nextTurtleId += 1
  return t

animate' :: Float 
         -> Float 
         -> (Float -> SequenceCommand a a) 
         -> SequenceCommand a a
animate' !distance !turtleSpeed callback =
   let !duration = distance / turtleSpeed
       !d' = if isNaN duration || isInfinite duration then 0 else duration
       --  if speed is 0 we use this as a "no animation" command from 
       --   user-space.
     in animate (abs d') callback

animate :: Float 
        -> (Float -> SequenceCommand a a) 
        -> SequenceCommand a a
animate !duration callback = do
   timeRemaining <- simTime -- simulation time to go
   let !availableTime = min timeRemaining duration
   --  Amount of time we have to complete the animation before we need to exit.
   let !timeQuot = if availableTime == 0 then 1 else availableTime / duration
   --  quotient of available time vs required time. Note that when the duration
   --   is 0 we say "don't do any animation"
   t <- callback timeQuot 
   --  Perform the calculation with the quotient for lerping
   decrementSimTime t availableTime
   --  Test to see if this is the end of our animation and if so exit early
   return t

-- | Runs two items in parallel then applies a semigroup combination operator
--   to the result of both.
--   This combination can only return if both A and B return. Compare to 
--   `alternateSequence` which can return if one returns.
combineSequence :: (Semigroup a, Monoid b)
                => SequenceCommand b a -- ^ Sequence /a/ to run.
                -> SequenceCommand b a -- ^ Sequence /b/ to run.
                -> SequenceCommand b a 
                    -- ^ New sequence of A and B in parallel.
combineSequence a b = do
  (aVal, bVal) <- runParallel a b
  combo aVal bVal
  where combo (Right x) (Right y) = return (x <> y)
        combo (Left x) (Right _)  = throwE x
        combo (Right _) (Left y)  = throwE y
        combo (Left x) (Left y)   = throwE (x <> y)

-- | Runs two items in sequence, returns the result of /a/ if /a/ passes,
--   otherwise returns the results of /b/. The implication of this is that only
--   the result of a will be returned while animating, and b when animation is
--   finished.
alternateSequence :: Monoid b
                  => SequenceCommand b a -- ^ Sequence /a/ to run.
                  -> SequenceCommand b a -- ^ Sequence /b/ to run.
                  -> SequenceCommand b a
alternateSequence a b = do
  (aVal, bVal) <- runParallel a b
  combo aVal bVal
  where combo (Right x) _       = return x
        combo _ (Right y)       = return y
        combo (Left x) (Left y) = throwE (x <> y)

-- | Given two sequences /a/ and /b/, instead of running them both as separate 
--   animations, run them both in parallel!
runParallel :: Monoid e 
            => SequenceCommand b a -- ^ Sequence /a/ to run.
            -> SequenceCommand d c -- ^ Sequence /b/ to run.
            -> SequenceCommand e (Either b a, Either d c)
               -- ^ New sequence of A and B which returns both results.
runParallel a b = do

  startSimTime <- use totalSimTime

  s <- lift get
  let (aVal, s') = processTurtle a s
  lift $ put s'
  
  aSimTime <- use totalSimTime

  let (bVal, s'') = processTurtle b (s' & totalSimTime .~ startSimTime)
  
  lift $ put s''
  
  bSimTime <- use totalSimTime
 
  -- No subsequent animation can proceed until the longest animation completes.
  -- We take the remaining animation time to the remaining time of the longest 
  -- running animation.
  totalSimTime .= min aSimTime bSimTime

  -- Now we must test the remaining sim time. The above calls might have
  -- succeeded while still exhausting our remaining time -- which as far as
  -- animating is concerned is the same as not succeeding at all!
  decrementSimTime mempty 0 
  return (aVal, bVal)

-- | Calls our early exit and fails the callback. No calculations will be
--   performed beyond this call.
failSequence :: b -> SequenceCommand b a
failSequence = throwE
