{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_HADDOCK hide #-}
module Graphics.WorldTurtle.Internal.Sequence
  ( Turtle 
  , TSC
  , SequenceCommand
  , defaultTSC
  , processTurtle
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
  ) where

import Graphics.WorldTurtle.Internal.Turtle

import Graphics.Gloss.Data.Picture (Picture)

import Control.Applicative (empty)
import Control.Monad (when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State.Strict

import Control.Lens
    ( (&), (^.), use, (%=), (%~), (+=), (.=), (.~), makeLenses )
import Control.Monad.IO.Class (liftIO)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

-- | State Monad that takes our `TSC` type as its state object.
type TurtleState = StateT TSC IO

-- | Maybe Monad on top of the State Monad of form @SequenceCommand a@.
--   This represents a computation that can be "partial." I.E. we can only 
--   animate so much of the scene with the time given.
type SequenceCommand a = MaybeT TurtleState a

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
  , _turtles :: !(Map Turtle TurtleData) -- Collection of all turtles.
  , _nextTurtleId :: !Int -- ^ ID of next turtle to be generated.
  }

$(makeLenses ''TSC)

-- | Generates default parameter arguments.
defaultTSC :: Float -> TSC
defaultTSC givenTime = TSC 
           { _pics = mempty
           , _totalSimTime = givenTime
           , _turtles = Map.empty
           , _nextTurtleId = 0
           }

-- | Gets the remaining simulation time of the current turtle process.
-- The simulation time dictates how much time is remaining for an animation,
-- and it will be reduced as the animations play in sequence. Once this value
-- hits 0 the exit command will be called and the monad will stop processing.
simTime :: SequenceCommand Float
simTime = use totalSimTime

-- | Sets the simulation time in the state monad.
-- If the simulation time is <= 0 then this setter will immediately call the
-- exit function which will kill any further processing of the monad.
setSimTime :: Float -- ^ Time to set.
           -> SequenceCommand ()
setSimTime newTime = do
  let newTime' = max 0 newTime
  totalSimTime .= newTime'
  when (newTime' <= 0) empty

-- | Takes a value away form the current sim time and store the updated time.
-- See `setSimTime`.
decrementSimTime :: Float -- ^ Value to subtract from store simulation time.
                 -> SequenceCommand ()
decrementSimTime duration = simTime >>= \ t -> setSimTime (t - duration)

-- | Given a picture, adds it to the picture list.
addPicture :: Picture -- ^ Picture to add to our animation
           -> SequenceCommand ()
addPicture p = pics %= ($!) (p :)

-- | Given a sequence and a State, returns the result of the computation and the
--   final state of the computation of form @(r, s)@. When @r@ is @Just@, then 
--   the computation completed, otherwise the computation ended early due to
--   lack of time available (i.e. a partial animation).
processTurtle :: SequenceCommand a 
              -> TSC
              -> IO (Maybe a, TSC)
processTurtle commands tsc = 
  let drawS = runMaybeT $ decrementSimTime 0 >> commands
   in runStateT drawS tsc

-- | Given a computation to run and an amount of time to run it in, renders the
--   final "picture".
renderTurtle :: SequenceCommand a 
             -> Float 
             -> IO Picture
renderTurtle c f = do
  let t  = defaultTSC f
  (_, s) <- processTurtle c t
  return $ mconcat $ reverse (s ^. pics) ++ drawTurtles (s ^. turtles)

drawTurtles :: Map Turtle TurtleData -> [Picture]
drawTurtles m = drawTurtle <$> Map.elems m 

generateTurtle :: SequenceCommand Turtle
generateTurtle = do
  t <- Turtle <$> use nextTurtleId
  turtles %= Map.insert t defaultTurtle
  nextTurtleId += 1
  return t

animate' :: Float 
         -> Float 
         -> (Float -> SequenceCommand a) 
         -> SequenceCommand a
animate' distance turtleSpeed callback =
   let duration = distance / turtleSpeed
       d' = if isNaN duration || isInfinite duration then 0 else duration
       --  if speed is 0 we use this as a "no animation" command from 
       --   user-space.
     in animate (abs d') callback

animate :: Float 
        -> (Float -> SequenceCommand a) 
        -> SequenceCommand a
animate duration callback = do
   timeRemaining <- simTime -- simulation time to go
   let availableTime = min timeRemaining duration
   --  Amount of time we have to complete the animation before we need to exit.
   let timeQuot = if availableTime == 0 then 1 else availableTime / duration
   --  quotient of available time vs required time. Note that when the duration
   --   is 0 we say "don't do any animation"
   t <- callback timeQuot 
   --  Perform the calculation with the quotient for lerping
   decrementSimTime availableTime
   --  Test to see if this is the end of our animation and if so exit early
   return t

-- | Runs two items in parallel then applies a semigroup combination operator
--   to the result of both.
--   This combination can only return if both A and B return. Compare to 
--   `alternateSequence` which can return if one returns.
combineSequence :: Semigroup a
                => SequenceCommand a -- ^ Sequence /a/ to run.
                -> SequenceCommand a -- ^ Sequence /b/ to run.
                -> SequenceCommand a 
                    -- ^ New sequence of A and B in parallel.
combineSequence a b = do
  (aVal, bVal) <- runParallel a b
  combo aVal bVal
  where combo (Just x) (Just y)  = return (x <> y)
        combo _ _                = empty

-- | Runs two items in sequence, returns the result of /a/ if /a/ passes,
--   otherwise returns the results of /b/. The implication of this is that only
--   the result of a will be returned while animating, and b when animation is
--   finished.
alternateSequence :: SequenceCommand a -- ^ Sequence /a/ to run.
                  -> SequenceCommand a -- ^ Sequence /b/ to run.
                  -> SequenceCommand a
alternateSequence a b = do
  (aVal, bVal) <- runParallel a b
  combo aVal bVal
  where combo (Just x) _ = return x
        combo _ (Just y) = return y
        combo _ _        = empty

-- | Given two sequences /a/ and /b/, instead of running them both as separate 
--   animations, run them both in parallel!
runParallel :: SequenceCommand a -- ^ Sequence /a/ to run.
            -> SequenceCommand b -- ^ Sequence /b/ to run.
            -> SequenceCommand (Maybe a, Maybe b)
               -- ^ New sequence of A and B which returns both results.
runParallel a b = do

  startSimTime <- use totalSimTime

  s <- lift get
  -- Run the "A" animation
  (aVal, bVal, aSimTime, s'') <- liftIO $ processTurtle a s >>= \(aVal, s') -> do
    let aSimTime = s' ^. totalSimTime
    -- Run the "B" animation from the same time
    (bVal, s'') <- processTurtle b $ s' & totalSimTime .~ startSimTime
    return (aVal, bVal, aSimTime, s'')
    -- No subsequent animation can proceed until the longest animation completes.
    -- We take the remaining animation time to be the remaining time of the 
    -- longest running animation
  lift $ put $ s'' & totalSimTime %~ min aSimTime

  -- Now we must test the remaining sim time. The above calls might have
  -- succeeded while still exhausting our remaining time -- which as far as
  -- animating is concerned is the same as not succeeding at all!
  decrementSimTime 0 
  return (aVal, bVal)
