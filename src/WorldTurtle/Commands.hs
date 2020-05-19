{-|
Module      : WorldTurtle.Commands
Description : The commands used 
Copyright   : (c) Archibald Neil MacDonald, 2020
License     : BSD3
Maintainer  : FortOyer@hotmail.co.uk
Stability   : experimental
Portability : POSIX

This module is a collection of all the commands used to manipulate a turtle!

-}
module WorldTurtle.Commands
  (
  -- * Setting up a turtle.
  -- $executionDoc
    runTurtle
  , TurtleCommand
  -- Building a turtle.
  , Turtle
  , makeTurtle
  -- * Movement commands.
  , P.Point
  , forward
  , fd
  , backward
  , bk
  , left
  , lt
  , right
  , rt
  , WorldTurtle.Commands.circle
  , goto
  , setPosition
  , setHeading
  , setSpeed
  -- * Styling commands.
  , stamp
  , representation
  -- * Tell turtle's state.
  , position
  , heading
  , speed
  , penColor
  , penDown
  -- * Drawing state.
  , setPenColor
  , setPenDown
  , setRepresentation
  -- * Canvas commands.
  , clear
  ) where

import qualified WorldTurtle.Internal.Turtle as T
import WorldTurtle.Internal.Commands
import qualified WorldTurtle.Internal.Coords as P

import Graphics.Gloss.Data.Color (Color, white, black)
import Graphics.Gloss.Data.Display (Display (..))
import qualified Graphics.Gloss.Interface.Pure.Animate as G (animate)
import Graphics.Gloss.Data.Picture

import Control.Monad (when, void)
import Control.Applicative (pure, liftA2)
import Control.Lens

import Control.Monad.State

import Data.Maybe (fromMaybe)

-- $executionDoc
-- A

newtype TurtleCommand a = TurtleCommand 
  { 
    seqT :: SequenceCommand (AlmostVal ()) a 
  }

instance Functor TurtleCommand where
  fmap f (TurtleCommand a) = TurtleCommand $ fmap f a

instance Applicative TurtleCommand where
  pure a = TurtleCommand $ pure a
  liftA2 f (TurtleCommand a) (TurtleCommand b) = TurtleCommand $ liftA2 f a b

instance Monad TurtleCommand where
  (TurtleCommand a) >>= f = TurtleCommand $ a >>= \s -> seqT (f s)

instance Semigroup a => Semigroup (TurtleCommand a) where
  (TurtleCommand a) <> (TurtleCommand b) = 
    TurtleCommand $ combineSequence a b

{- |
Creates a new `Turtle` and displays it on the canvas. This turtle can then be
manipulated! For example, to create a turtle and then move the turtle forward:

   @
    main:: IO ()
    main = runTurtle $ do
      t <- makeTurtle
      forward t 90
   @
-}
makeTurtle :: TurtleCommand Turtle
makeTurtle = TurtleCommand generateTurtle

-- | This is the heart of the turtle system. When run, this command will draw
-- canvas and start animating the given `TurtleCommand`.
runTurtle :: TurtleCommand () -- ^ Command sequence to execute
           -> IO ()
runTurtle tc = G.animate display white iterate
     where display = InWindow "World Turtle" (800, 600) (400, 300)
           iterate f = renderTurtle (seqT tc) f

-- | Move the turtle backward by the specified @distance@, in the direction the 
--   turtle is headed.
backward :: Turtle -- ^ The turtle to move.
         -> Float -- ^ Distance to move the turtle.
         -> TurtleCommand ()
backward turtle d = forward turtle (-d)

-- | Shorthand for `backward`.
bk = backward

-- | Turn a turtle right by the given degrees amount.
right :: Turtle -- ^ The turtle to rotate.
      -> Float -- ^ Rotation amount to apply to turtle.
      -> TurtleCommand ()
right t r = left t (-r)

-- | Shorthand for `right`.
rt = right

-- | Move the turtle forward by the specified @distance@, in the direction the 
--   turtle is headed.
forward :: Turtle -- ^ The turtle to move.
        -> Float -- ^ Distance to move the turtle.
        -> TurtleCommand ()
forward turtle d = TurtleCommand $ do
    t <- tData_ turtle
    --  Get origin point
    let speed = t ^. T.speed
    animate' d speed $ \ q -> do
      let startP = t ^. T.position
      let heading = P.degToRad $ t ^. T.heading
      let vec = P.rotateV heading (d, 0)
      let endP = vec P.+ startP
      let midP = P.lerp q startP endP
      --  Get new endpoint via percentage
      when (t ^. T.penDown) $ do -- don't draw if pen isn't in down state
        addPicture $ color (t ^. T.penColor) $ line [startP, midP] 
        --  Draw line from startPoint to midPoint.
      turtLens_ turtle . T.position .= midP
      --  Update the turtle to a new position

-- | Shorthand for `forward`.
fd = forward

-- | Stamp a copy of the turtle shape onto the canvas at the current turtle 
--   position.
stamp :: Turtle -- ^ The turtle with the shape to be copied.
      -> TurtleCommand ()
stamp turtle = TurtleCommand $ tData_ turtle >>= addPicture . T.drawTurtle 

-- | Turn a turtle left by the given degrees amount.
left :: Turtle -- ^ The turtle to rotate.
     -> Float -- ^ Rotation amount to apply to turtle.
     -> TurtleCommand ()
left turtle r = TurtleCommand $ do
    t <- tData_ turtle
    let r' = P.normalizeDirection r
    animate' (P.degToRad r') (t ^. T.speed) $ \q -> do
      let heading = t ^. T.heading
      --let q' = if r > 0 then q else -q
      let newHeading = P.normalizeHeading $ heading + q * r'
      --  Get new heading via percentage
      turtLens_ turtle . T.heading .= newHeading

-- | Shorthand for `left`.
lt = left

circle  :: Turtle -- ^ Turtle to move in a circle.
        -> Float -- ^ Radius of the circle.
        -> Float -- ^ Angle to travel in degrees. 
                 -- For example: @360@ for a full circle or @180@ for a 
                 -- semicircle.
        -> TurtleCommand ()
circle turtle radius r = TurtleCommand $ do
  t <- tData_ turtle
  let r' = P.normalizeHeading r
  animate' (radius * P.degToRad r') (t ^. T.speed) $ \q -> do
    let startAngle = t ^. T.heading
    let p = t ^. T.position
    let angle = r' * q
    when (t ^. T.penDown) $ do -- don't draw if pen isn't in down state
      let lPic  = translate (fst p) (snd p)
                $ rotate (90 - startAngle)
                $ translate (-radius) 0
                $ color (t ^. T.penColor)
                $ arc 0 angle radius
      addPicture lPic

    let cS = cos $ P.degToRad $ 90 - startAngle
    let sS = sin $ P.degToRad $ 90 - startAngle
    let cA = cos $ P.degToRad $ (90 - startAngle - angle)
    let sA = sin $ P.degToRad $ (90 - startAngle - angle)
    let rx =   radius * (cA - cS) + fst p
    let ry = -(radius * (sA - sS) + snd p)

    -- Update the turtle with the new values.
    let ts = turtLens_ turtle
    ts . T.heading .= startAngle + angle
    ts . T.position .= (rx, ry)

-- | Returns the turtle's current position.
--   Default (starting) position is @(0, 0)@.
position :: Turtle -- ^ Turtle to query.
         -> TurtleCommand P.Point -- ^ Returned current point.
position = getter_ (0, 0) T.position

-- | Warps the turtle to a new position.
--   The turtle jumps to this new position with no animation. If the pen is down
--   then a line is drawn.
goto :: Turtle -- ^ Turtle to query.
     -> P.Point -- ^ Position to warp to.
     -> TurtleCommand ()
goto turtle point = TurtleCommand $ do
  t <- tData_ turtle
  let startP = t ^. T.position
  when (t ^. T.penDown) $ addPicture 
                        $ color (t ^. T.penColor) 
                        $ line [startP, point] 
  turtLens_ turtle . T.position .= point

-- | Alias of `goto`.
setPosition = goto

-- | Returns the turtle's heading.
--   
--   @0@ is along the positive x-axis, going anticlockwise. So:
--
--   * East is @0@ degrees.
--   * North is @90@ degrees.
--   * West is @180@ degrees.
--   * South is @270@ degrees.
--
--   The default heading is North (@90@ degrees).
heading :: Turtle -- ^ Turtle to query.
         -> TurtleCommand Float -- ^ Returned heading as angle in degrees.
heading = getter_ 0 T.heading

-- | Sets the turtle's heading. See `heading`.
setHeading :: Turtle -- ^ Turtle to set.
           -> Float -- ^ Heading to apply.
           -> TurtleCommand ()
setHeading = setter_ T.heading

-- | Returns the turtle's pen color.
--   The color of the turtle's pen.The default color is @black@.
penColor :: Turtle -- ^ Turtle to query.
         -> TurtleCommand Color -- ^ Returned current pen color.
penColor = getter_ black T.penColor

-- | Set the turtle's pen color.
--  See `penColor`.
setPenColor :: Turtle -- ^ Turtle to modify.
            -> Color -- ^ New pen color to apply
            -> TurtleCommand ()
setPenColor = setter_ T.penColor

-- | Returns whether the turtle's pen is down.
--   When the turtle's pen is down it will draw a line when it moves.
--   The default value is @true@.
penDown :: Turtle -- ^ Turtle to query.
         -> TurtleCommand Bool -- ^ True if pen is down, false if not.
penDown = getter_ False T.penDown

-- | Sets the turtle's pen is down.
--   See `penDown`.
setPenDown :: Turtle -- ^ Turtle to modify.
           -> Bool -- ^ New state for pen flag.
           -> TurtleCommand ()
setPenDown = setter_ T.penDown

-- | Returns whether the turtle's current speed.
--   Speed is is @distance@ per second.
-- The default value is @TODO - DECIDE ON A DEFAULT VALUE@.
speed :: Turtle -- ^ Turtle to query.
      -> TurtleCommand Float
speed = getter_ 0 T.speed

-- | Sets the turtle's speed.
--   See `speed`.
setSpeed :: Turtle -- ^ Turtle to modify.
         -> Float -- ^ New speed.
         -> TurtleCommand ()
setSpeed = setter_ T.speed

-- | Gets the turtle's representation as a Gloss `Picture`.
representation :: Turtle -- ^ Turtle to query.
               -> TurtleCommand Picture
representation = getter_ blank T.representation

{- | Sets the turtle's representation to a Gloss `Picture`.
   See `representation`.
   For example, to set the turtle as a red circle:
   
   @
    import WorldTurtle
    import qualified Graphics.Gloss.Data.Picture as G

    myCommand :: TurtleCommand ()
    myCommand = do
      t <- makeTurtle
      setPenColor t red
      setRepresentation t $ G.color red $ G.circleSolid 10
      forward t 90
   @
-}
setRepresentation :: Turtle -- ^ Turtle to mutate.
                  -> Picture
                  -> TurtleCommand ()
setRepresentation = setter_ T.representation

-- | Clears all drawings form the canvas. Does not alter any turtle's state.
clear :: TurtleCommand ()
clear = TurtleCommand $ pics .= []

{-
   Here be dirty helper functions:
-}

-- | Looks up the turtle data for the given turtle in the state monad.
turtLens_ t = turtles . at t . _Just 

-- | This is a helper function for our getter commands.
--   It takes a default value, the lense to compose, and the turtle to inspect.
getter_ def lens t = 
  TurtleCommand $ fromMaybe def <$> preuse (turtLens_ t . lens)

-- | This is a helper function that extracts the turtle data for a given turtle.
tData_ = seqT <$> getter_ T.defaultTurtle id

-- | This is a helper function for our setter commands
-- It takes a lens, the turtle to inspect, and the value to apply.
setter_ lens t val = 
  TurtleCommand $ turtLens_ t . lens .= val