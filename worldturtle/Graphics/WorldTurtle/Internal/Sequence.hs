{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE BangPatterns #-}
module Graphics.WorldTurtle.Internal.Sequence
  ( Turtle 
  , TSC
  , SequenceCommand
  , AlmostVal
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

import Control.Monad.Cont
import Control.Monad.State

import Control.Lens

import Data.Void (Void, absurd)
import Data.Maybe (isNothing, isJust)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

-- | AlmostVal represents a computation that can "almost" complete. Either
--   There is enough time to solve the computation, or the computation needs
--   to exit early as there is not enough time to fully run the computation.
type AlmostVal a = Maybe a

-- | State Monad that takes our `TSC` type as its state object.
type TurtleState b = State (TSC b)

-- | Continuation Monad on top of the State Monad of form @SequenceCommand b a@.
--   /b/ is the final return type of the entire Monad sequence - this is what 
--   will be returned if/when we need to exit early from anywhere in a great big
--   sequence of steps. /a/ is the return type of the current step of the 
--   animation sequence. That is: what will be passed into the next step.
type SequenceCommand b a = ContT b (TurtleState b) a

-- Careful of editing the Turtle comment below as it is public docs!
-- Really "Turtle" is just a handle to internal TurtleData. It is a key that
-- looks up TurtleData in a map. Since Turtle is exposed to the user-level we 
-- do not document it in this way however.

-- | The Turtle that is drawn on the canvas! Create a new turtle using 
-- `Graphics.WorldTurtle.Commands.makeTurtle`.
newtype Turtle = Turtle Int deriving (Eq, Ord)

data TSC b = TSC
  { _pics :: ![Picture] -- ^ All pictures that make up the current canvas
  , _exitCall :: SequenceCommand b b -- ^ Stop drawing call for animations
  , _totalSimTime :: !Float -- ^ Remaining available for animating
  , _turtles :: Map Turtle TurtleData  -- Collection of all turtles.
  , _nextTurtleId :: !Int -- ^ ID of next turtle to be generated.
  }

$(makeLenses ''TSC)

-- | Generates default parameter arguments. The TSC returned by this value
-- must never be used for sequencing as the exitCall is undefined and will only
-- be defined in the setup stage of the animation process.
defaultTSC :: Float -> TSC b
defaultTSC givenTime = TSC 
           { _pics = []
           , _totalSimTime = givenTime
           , _exitCall = error "Exit called but not defined in animation."
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
setSimTime :: Float -> SequenceCommand b ()
setSimTime newTime = do
  let newTime' = max 0 newTime
  totalSimTime .= newTime'
  when (newTime' <= 0) failSequence

-- | Takes a value away form the current sim time and store the updated time.
-- See `setSimTime`.
decrementSimTime :: Float -- ^ Value to subtract from store simulation time. 
                 -> SequenceCommand b ()
decrementSimTime duration = simTime >>= setSimTime . (flip (-) duration)

-- | Given a picture, adds it to the picture list.
addPicture :: Picture -- ^ Picture to add to our animation
           -> SequenceCommand b ()
addPicture p = pics %= (p :)

-- | Never call an animation directly, always call this instead!
-- This is part of our setup stage to inject the exit call into the animation
-- before running the animation. What is returned by this class is either
-- the completed animation or an early exit. 
--
-- We take our command and an exit call, and store the exit in the state monad 
-- then execute the command.
-- The return value is either a `Nothing` which means the exit was called early
-- or a `Just a` which is the monad successfully completed.
exitCondition :: SequenceCommand (AlmostVal a) a -- ^ Animation passed in.
              -> SequenceCommand (AlmostVal a) (AlmostVal a)
exitCondition commands = callCC $ \exit -> do
    exitCall .= exit Nothing
    decrementSimTime 0 -- In case we are already at a time of 0.
    Just <$> commands

processTurtle :: SequenceCommand (AlmostVal a) a 
              -> TSC (AlmostVal a)
              -> (AlmostVal a, TSC (AlmostVal a))
processTurtle commands tsc = 
  let drawS = runContT (exitCondition commands) return
   in runState drawS tsc

renderTurtle :: SequenceCommand (AlmostVal a) a -> Float ->  Picture
renderTurtle c f = let (_, s) = processTurtle c (defaultTSC f)
                    in pictures $ s ^. pics ++ drawTurtles (s ^. turtles)

drawTurtles :: Map Turtle TurtleData -> [Picture]
drawTurtles m = fmap drawTurtle $ Map.elems m 

generateTurtle :: SequenceCommand b Turtle
generateTurtle = do
  t <- Turtle <$> use nextTurtleId
  turtles %= Map.insert t defaultTurtle
  nextTurtleId += 1
  return t

animate' :: Float 
         -> Float 
         -> (Float -> SequenceCommand b a) 
         -> SequenceCommand b a
animate' !distance !turtleSpeed callback =
   let !duration = distance / turtleSpeed
       !d' = if isNaN duration || isInfinite duration then 0 else duration
       --  if speed is 0 we use this as a "no animation" command from 
       --   user-space.
     in animate (abs d') callback

animate :: Float -> (Float -> SequenceCommand b a) -> SequenceCommand b a
animate !duration callback = do
   timeRemaining <- simTime -- simulation time to go
   let !availableTime = min timeRemaining duration
   --  Amount of time we have to complete the animation before we need to exit.
   let !timeQuot = if availableTime == 0 then 1 else availableTime / duration
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
                => SequenceCommand b a -- ^ Sequence /a/ to run.
                -> SequenceCommand b a -- ^ Sequence /b/ to run.
                -> SequenceCommand b a 
                    -- ^ New sequence of A and B in parallel.
combineSequence a b = do
  (!aVal, !bVal) <- runParallel a b
  -- If either attempt failed, we fail also.
  when (isNothing aVal || isNothing bVal) failSequence

  -- Everything is hunky dory so we continue on into the next bind of the monad.
  let (Just !aVal') = aVal
  let (Just !bVal') = bVal
  return $ aVal' <> bVal'

-- | Runs two items in sequence, returns the result of `a` if `a` passes,
--   otherwise returns the results of `b`. The implication of this is that only
--   the result of a will be returned while animating, and b when animation is
--   finished.
alternateSequence :: SequenceCommand b a -- ^ Sequence @a@ to run.
                  -> SequenceCommand b a -- ^ Sequence @b@ to run.
                  -> SequenceCommand b a
alternateSequence a b = do
  (!aVal, !bVal) <- runParallel a b
  
  -- If both values failed we fail also.
  when (isNothing aVal && isNothing bVal) failSequence

  -- If A passes, return the value of A, otherwise return the value of B.
  if isJust aVal 
    then let (Just !aVal') = aVal in return $! aVal'
    else let (Just !bVal') = bVal in return $! bVal'

-- | Given two sequences /a/ and /b/, instead of running them both as separate 
--   animations, run them both in parallel!
runParallel :: SequenceCommand c a -- ^ Sequence /a/ to run.
            -> SequenceCommand c b -- ^ Sequence /b/ to run.
            -> SequenceCommand c (AlmostVal a, AlmostVal b)
               -- ^ New sequence of A and B which returns both results.
runParallel a b = do
  startSimTime <- use totalSimTime
  parentExitCall <- use exitCall

  -- Run A, and return back to this point when/if it fails.
  aVal <- callCC $ \ exitFromA -> do
    exitCall .= exitFromA Nothing
    Just <$> a

  aSimTime <- use totalSimTime
  
  -- Run B, and return back to this point when/if it fails.
  bVal <- callCC $ \ exitFromB -> do
    exitCall .= exitFromB Nothing
    totalSimTime .= startSimTime -- restart sim time back to initial.
    Just <$> b

  bSimTime <- use totalSimTime

  -- No subsequent animation can proceed until the longest animation completes.
  -- We take the remaining animation time to the remaining time of the longest 
  -- running animation.
  totalSimTime .= min aSimTime bSimTime

  exitCall .= parentExitCall  -- Let us exit properly again!

  -- Now we must test the remaining sim time. The above calls might have
  -- succeeded while still exhausting our remaining time -- which as far as
  -- animating is concerned is the same as not succeeding at all!
  decrementSimTime 0

  return $! (aVal, bVal)

-- | Calls our early exit and fails the callback. No calculations will be
--   performed beyond this call.
failSequence :: SequenceCommand b a
failSequence = do
  ex <- use exitCall
  _ <- ex
  -- We can never reach this point with our call to `ex`. So the return type
  -- can be whatever we want it to be. Let's go crazy! 
  let (Just x) = (Nothing :: Maybe Void)
   in absurd x