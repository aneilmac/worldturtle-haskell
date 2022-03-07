{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_HADDOCK hide #-}
module Graphics.WorldTurtle.Internal.Sequence
  ( Turtle 
  , TSC
  , SequenceCommand
  , SequencePause
  , defaultTSC
  , startSequence
  , resumeSequence
  , renderPause
  , decrementSimTime
  , addPicture
  , pics
  , turtles
  , generateTurtle
  , animate'
  , runParallel
  ) where

import Graphics.WorldTurtle.Internal.Turtle
    ( defaultTurtle, drawTurtle, TurtleData )

import Graphics.Gloss.Data.Picture (Picture, pictures)

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict
    ( StateT, get, put, evalStateT )

import Control.Lens
    ( (.~), (&), (+=), (^.), (%=), (.=), use, makeLenses )

import Control.Monad.Coroutine (Coroutine(..))
import Control.Monad.Coroutine.SuspensionFunctors (Request(..), request )

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

-- | State Monad that takes our `TSC` type as its state object.
type TurtleState = StateT TSC IO

-- | Maybe Coroutine on top of the State Monad of form @SequenceCommand a@.
--   This represents a computation that can be "paused." I.E. we can only 
--   animate so much of the scene with the time given.
type SequenceCommand a = Coroutine (Request TSC Float) TurtleState a
type SequencePause a = Either (Request TSC Float (SequenceCommand (a, TSC))) (a, TSC)

-- Careful of editing the Turtle comment below as it is public docs!
-- Really "Turtle" is just a handle to internal TurtleData. It is a key that
-- looks up TurtleData in a map. Since Turtle is exposed to the user-level we 
-- do not document it in this way however.

-- | The Turtle that is drawn on the canvas! Create a new turtle using 
-- `Graphics.WorldTurtle.Commands.makeTurtle`.
newtype Turtle = Turtle Int deriving (Eq, Ord)

data TSC = TSC
  { _pics :: ![Picture] -- ^ All pictures currently drawn this sequence.
  , _finalPics :: ![Picture] -- ^ All pictures that have successfuly drawn in previous sequences.
  , _simTime :: !Float -- ^ Total simulation time.
  , _turtles :: !(Map Turtle TurtleData) -- Collection of all turtles.
  , _nextTurtleId :: !Int -- ^ ID of next turtle to be generated.
  }

$(makeLenses ''TSC)

-- | Generates default parameter arguments.
defaultTSC :: TSC
defaultTSC = TSC 
           { _pics = mempty
           , _finalPics = mempty
           , _simTime = 0
           , _turtles = Map.empty
           , _nextTurtleId = 0
           }

-- | Attempts to reduce our simulation time by @d@. 
--   If we run out of simualtion time, this Monad whill yield,
--   allowing for a render, before it continues once again.
decrementSimTime :: Float -- ^ Decrement simulation time by this amount. 
                 -> SequenceCommand Bool -- ^ True if simulation yielded, false otherwise.
decrementSimTime d = do
  t <- lift $ use simTime
  let t' = max 0 (t - d)
  let outOfTime = t' <= 0
  lift $ simTime .= t' 
  when outOfTime $ do
    --- Before we yield, take the chance to concat our final pics.
    lift $ finalPics %= \f -> [pictures f]
    -- If we have run out of time,
    -- pause the continuation to allow for
    -- a render, then resume.
    s <- lift get

    delta <- request s
    lift $ simTime += delta
  return outOfTime

-- | Given a picture, adds it to the picture list.
addPicture :: Picture -- ^ Picture to add to our animation
           -> SequenceCommand ()
addPicture p = lift $ pics %= ($!) (p :)

-- | Given a sequence, returns the result of the computation and the
--   final state of the computation of form @(r, s)@. When @r@ is @Just@, then 
--   the computation completed, otherwise the computation ended early due to
--   lack of time available (i.e. a partial animation).
startSequence :: TSC
              -> SequenceCommand a -- ^ Commands to execute
              -> IO (SequencePause a)
startSequence tsc commands = evalStateT (resume commands') tsc
  where commands' = do
          _ <- decrementSimTime 0 -- Kick off an immediate Yield.
          a <- commands
          g <- lift get
          return (a, g)

runSequence :: TSC
            -> SequenceCommand (a, TSC) -- ^ Commands to execute
            -> IO (SequencePause a)
runSequence tsc commands = evalStateT (resume commands) tsc

resumeSequence :: Float -> SequencePause a -> IO (SequencePause a)
resumeSequence delta (Left (Request tsc response)) = runSequence tsc $ response delta
resumeSequence _ a = return a

renderPause :: SequencePause a -> Picture
renderPause sq = renderTurtle $ stateForPause sq

stateForPause :: SequencePause a -> TSC
stateForPause (Left (Request s _)) = s
stateForPause (Right (_, s)) = s

-- | Exctracts the image frame from the current turtle state.
renderTurtle :: TSC -> Picture
renderTurtle t = mconcat $ 
  (t ^. finalPics) ++
  (t ^. pics) ++ 
  drawTurtles (t ^. turtles)

drawTurtles :: Map Turtle TurtleData -> [Picture]
drawTurtles m = drawTurtle <$> Map.elems m 

generateTurtle :: SequenceCommand Turtle
generateTurtle = lift $ do
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
  in do 
    t <- animate (abs d') callback
    -- If we reach this point, then a "full" animation
    -- has completed successfully. We move the drawn images
    -- from our temp pics list to our finalPics list, and 
    -- empty the temp pics list.
    lift $ do
      p <- use pics
      finalPics %= ($!) (++ p)
      pics .= mempty
    return t

animate :: Float 
        -> (Float -> SequenceCommand a) 
        -> SequenceCommand a
animate duration callback = do
  oldState <- lift get

  timeRemaining <- lift $ use simTime -- simulation time to go
  let availableTime = min timeRemaining duration
  --  Amount of time we have to complete the animation before we need to exit.
  let timeQuot = if availableTime == 0 then 1 else availableTime / duration
  --  quotient of available time vs required time. Note that when the duration
  --   is 0 we say "don't do any animation"
  
  t <- callback timeQuot 
   
  --  Perform the calculation with the quotient for lerping
  outOfTime <- decrementSimTime availableTime

    -- When out of time has occurred, all progress that has been made this `animate` call
    -- is thrown away after being drawn. We re-attempt the animation, with more simulation
    -- time available so that the sequence goes "further."
  if outOfTime then do
      let oldTime = oldState ^. simTime
      newTime <- lift $ use simTime
      let time = newTime + oldTime
      lift $ put $ oldState & simTime .~ time
      animate duration callback
    else 
      return t


-- | Given two sequences /a/ and /b/, instead of running them both as separate 
--   animations, run them both in parallel!
runParallel :: (a -> b -> SequenceCommand c)
            -> SequenceCommand a -- ^ Sequence /a/ to run.
            -> SequenceCommand b -- ^ Sequence /b/ to run.
            -> SequenceCommand c
               -- ^ New sequence of A and B which returns both results.
runParallel f a b = 
  let a' = a >>= \ax -> lift get >>= \g -> return (ax, g)
      b' = b >>= \bx -> lift get >>= \g -> return (bx, g)
   in runParallel_ f a' b' 

-- | Main body for parallel animations. Runs one sequence, rewinds, then 
--   runs the other sequence, we then attempt to continue our calculations.
runParallel_ :: (a -> b -> SequenceCommand c)
            -> SequenceCommand (a, TSC) -- ^ Sequence /a/ to run.
            -> SequenceCommand (b, TSC) -- ^ Sequence /b/ to run.
            -> SequenceCommand c
               -- ^ New sequence of A and B which returns both results.
runParallel_ f a b = do
  startSimTime <- lift $ use simTime
  s <- lift get
  
  -- Run the "A" animation
  aVal <- liftIO $ runSequence s a
  let s' = grabState aVal
  let aTime = s' ^. simTime

  -- Run the "B" animation, with a reset time.
  let s'' = s' & simTime .~ startSimTime
  bVal <- liftIO $ runSequence s'' b
  let s''' = grabState bVal
  let bTime = s''' ^. simTime

  -- Test to see if we need to yield.
  let elapsedTime = min aTime bTime
  lift $ put (s''' & simTime .~ elapsedTime)
  outOfTime <- decrementSimTime 0
  
  -- If we were out of time, redo the operation.
  newTime <- lift $ use simTime
  if outOfTime then do
    let time = newTime + startSimTime
    lift $ put $ s & simTime .~ time
    runParallel_ f a b
  else do
    combinePauses_ f newTime aVal bVal

  where grabState (Left (Request s _)) = s
        grabState (Right (_, s)) = s

combinePauses_ :: (a -> b ->  SequenceCommand c) -> Float -> SequencePause a -> SequencePause b -> SequenceCommand c
combinePauses_ f _ (Right (a, _)) (Right (b, _)) = f a b
combinePauses_ f d (Right (a, _)) (Left (Request _ y)) = y d >>= f a . fst
combinePauses_ f d (Left (Request _ x)) (Right (b, _)) = x d >>= (`f` b) . fst
combinePauses_ f d (Left (Request _ x)) (Left (Request _ y)) = runParallel_ f (x d) (y d)