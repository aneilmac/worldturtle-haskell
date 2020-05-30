{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-|
Module      : Graphics.WorldTurtle.Commands
Description : The commands used 
Copyright   : (c) Archibald Neil MacDonald, 2020
License     : BSD3
Maintainer  : FortOyer@hotmail.co.uk
Stability   : experimental
Portability : POSIX

This module contains all commands used to create, move and 
manipulate a turtle.

-}
module Graphics.WorldTurtle.Commands
  (
  -- * Types
    Turtle
  , P.Point
  -- * WorldCommand commands.
  -- ** Creating a turtle.
  , makeTurtle
  , makeTurtle'
  -- ** Canvas commands.
  , clear
  , sleep
  -- * TurtleCommand commands.
  -- ** Movement commands.
  , forward
  , fd
  , backward
  , bk
  , left
  , lt
  , right
  , rt
  , Graphics.WorldTurtle.Commands.circle
  , Graphics.WorldTurtle.Commands.arc
  , goto
  , setPosition
  , home
  , setHeading
  , setSpeed
  , setRotationSpeed
  -- * Styling commands.
  , stamp
  , representation
  -- ** Query turtle's state.
  , position
  , heading
  , speed
  , rotationSpeed
  , penColor
  , penDown
  , penSize
  , visible
  -- ** Mutate turtle's state.
  , branch
  , setPenColor
  , setPenDown
  , setPenSize
  , setRepresentation
  , setVisible
  -- * Common constants
  , east
  , north
  , west
  , south
  ) where

import Data.Maybe (fromMaybe)

import Control.Lens
import Control.Monad

import Graphics.WorldTurtle.Shapes

import Graphics.WorldTurtle.Internal.Commands
import Graphics.WorldTurtle.Internal.Sequence

import qualified Graphics.WorldTurtle.Internal.Turtle as T
import qualified Graphics.WorldTurtle.Internal.Coords as P

import Graphics.Gloss.Data.Color (Color, black)

import Graphics.Gloss.Data.Picture

{- |
Creates a new `Turtle` and displays it on the canvas. This turtle can then be
manipulated! For example, to create a turtle and then move the turtle forward:

   @
    main:: IO ()
    main = runWorld $ do
      t <- makeTurtle
      t >/> forward 90
   @

The default turtle starts at position (0, 0) and is orientated `north`.

-}
makeTurtle :: WorldCommand Turtle
makeTurtle = WorldCommand generateTurtle

{-| This variant of `makeTurtle` takes a starting position, a starting 
    orientation, and a color to apply to the turtle and the turtle's pen.

    @
      myCommand :: WorldCommand ()
      myCommand = do
        t1 <- makeTurtle' (0, 0)  0 green
        t2 <- makeTurtle' (0, 0) 90 red
        (t1 >/> forward 90) \<|\> (t2 >/> forward 90)
    @

    See `makeTurtle`.
-}
makeTurtle' :: Point -- ^ Initial position of the turtle.
            -> Float -- ^ Initial heading of the turtle.
            -> Color -- ^ Color of the turtle and the turtle's pen.
            -> WorldCommand Turtle -- ^ The generated turtle.
makeTurtle' p f c = WorldCommand $ do 
  turtle <- generateTurtle
  let ts = turtLens_ turtle
  ts . T.position       .= p
  ts . T.heading        .= f
  ts . T.representation .= turtleArrow black c
  ts . T.penColor       .= c
  return turtle

-- | Move the turtle backward by the specified @distance@, in the direction the 
--   turtle is headed.
backward :: Float -- ^ Distance to move the turtle.
         -> TurtleCommand ()
backward !d = forward (-d)

-- | Shorthand for `backward`.
bk :: Float -> TurtleCommand ()
bk = backward

calculateNewPointF_ :: P.Point -- ^ Starting point
                    -> Float -- ^ Distance
                    -> Float -- ^ Heading in degrees.
                    -> Float -- ^ coefficient [0, 1]
                    -> P.Point
calculateNewPointF_ !p !d !h !q = P.lerp q p endP
  where !vec = P.rotateV (P.degToRad h) (d, 0)
        !endP = vec P.+ p

-- | Move the turtle forward by the specified @distance@, in the direction the 
--   turtle is headed.
forward :: Float -- ^ Distance to move the turtle.
        -> TurtleCommand ()
forward !d = TurtleCommand $ \ turtle -> do
    t <- tData_ turtle
    --  Get origin point
    animate' d (t ^. T.speed) $ \ q -> do
      --  Get new endpoint via percentage
      let !startP = t ^. T.position
      let !midP = calculateNewPointF_ startP d (t ^. T.heading) q
      when (t ^. T.penDown) $ do -- don't draw if pen isn't in down state
        addPicture $ color (t ^. T.penColor) 
                   $ thickLine startP midP (t ^. T.penSize)
        --  Draw line from startPoint to midPoint.
      turtLens_ turtle . T.position .= midP
      --  Update the turtle to a new position

-- | Shorthand for `forward`.
fd :: Float -> TurtleCommand ()
fd = forward

-- | Stamp a copy of the turtle shape onto the canvas at the current turtle 
--   position.
stamp :: TurtleCommand ()
stamp = TurtleCommand $ \ turtle -> tData_ turtle >>= addPicture . T.drawTurtle 

-- | Turn a turtle right by the given degrees amount.
right :: Float -- ^ Rotation amount to apply to turtle.
      -> TurtleCommand ()
right = rotateTo_ True

-- | Shorthand for `right`.
rt :: Float -> TurtleCommand ()
rt = right

-- | Turn a turtle left by the given degrees amount.
left :: Float -- ^ Rotation amount to apply to turtle.
     -> TurtleCommand ()
left = rotateTo_ False

-- | Shorthand for `left`.
lt :: Float -> TurtleCommand ()
lt = left

rotateTo_ :: Bool -- ^ Bias decides in which direction rotation happens.
          -> Float -- ^ Amount to rotate by
          -> TurtleCommand ()
rotateTo_  rightBias !r = TurtleCommand $ \ turtle -> do
    t <- tData_ turtle
    let r' = P.normalizeHeading r
    animate' (P.degToRad r') (t ^. T.rotationSpeed) $ \q -> do
      let !h = t ^. T.heading
      let !newHeading = P.normalizeHeading $ if rightBias then h - q * r'
                                                          else h + q * r'
      --  Get new heading via percentage
      turtLens_ turtle . T.heading .= newHeading

-- | Draw a circle with a given @radius@. The center is @radius@ units left of 
--   the @turtle@ if positive. Otherwise  @radius@ units right of the @turtle@ 
--   if negative.
--
--   The circle is drawn in an anticlockwise direction if the radius is 
--   positive, otherwise, it is drawn in a clockwise direction.
--
--   Circle is an alias for @circle r = arc r 360@.
circle :: Float -- ^ Radius of the circle.
       -> TurtleCommand ()
circle radius = Graphics.WorldTurtle.Commands.arc radius 360

-- | Draws an arc starting from a given starting point on the edge of the
--   circle.
drawCircle_ :: P.Point -- ^ Point on edge of circle to start from
            -> Float -- ^ Radius of circle
            -> Float -- ^ Absolute starting angle in degrees
            -> Float -- ^ Rotation amount about radius in degrees
            -> Float -- ^ Line thickness (penSize)
            -> Color -- ^ Color of circle 
            -> Picture -- ^ Resulting circle
drawCircle_ !p !radius !startAngle !endAngle !pSize !pColor = 
 translate (fst p) (snd p) $ rotate (180 - startAngle)
                           $ translate (-radius) 0
                           $ color pColor
                           $ scale (if radius >= 0 then 1 else -1) 1
                           $ thickArc 0 (endAngle) (abs radius) pSize

-- Calculates the next position of a turtle on a circle.
calculateNewPointC_ :: P.Point -- ^ Point on edge of circle
                    -> Float -- ^ Radius of circle
                    -> Float -- ^ Absolute starting angle in degrees
                    -> Float -- ^ Rotation amount about radius in degrees
                    -> P.Point -- ^ Resulting new point
calculateNewPointC_ !p !radius !startAngle !angle = (px, py)
  where !px = fst p - (radius * (cos a - cos s))
        !py = snd p - (radius * (sin a - sin s))
        !s = P.degToRad startAngle
        !a = P.degToRad $ if radius >= 0 then startAngle + angle
                                         else startAngle - angle

-- | Draw an arc with a given @radius@. The center is @radius@ units left of the
--   @turtle@ if positive. Otherwise  @radius@ units right of the @turtle@ if 
--   negative.
--
--   The arc is drawn in an anticlockwise direction if the radius is positive,
--   otherwise, it is drawn in a clockwise direction.
arc  :: Float -- ^ Radius of the circle.
     -> Float -- ^ Angle to travel in degrees. 
                 -- For example: @360@ for a full circle or @180@ for a 
                 -- semicircle.
     -> TurtleCommand ()
arc !radius !r = TurtleCommand $ \turtle -> do
  t <- tData_ turtle
  let !r' = P.normalizeHeading r
  animate' (abs radius * P.degToRad r') (t ^. T.speed) $ \ q -> do
    let !startAngle = t ^. T.heading + 90
    let !p = t ^. T.position
    let !angle = r' * q
    when (t ^. T.penDown) $ do -- don't draw if pen isn't in down state
      addPicture $! drawCircle_ p radius startAngle angle 
                    (t ^. T.penSize) (t ^. T.penColor)

    -- Update the turtle with the new values.
    let ts = turtLens_ turtle
    ts . T.heading .= P.normalizeHeading (if radius >= 0
                                          then startAngle - 90 + angle
                                          else startAngle - 90 - angle)
    

    let !p' = calculateNewPointC_ p radius startAngle angle
    ts . T.position .= p'

-- | Returns the turtle's current position.
--   Default (starting) position is @(0, 0)@.
position :: TurtleCommand P.Point -- ^ Returned current point.
position = getter_ (0, 0) T.position

-- | Warps the turtle to its starting position @(0, 0)@ and resets the
--   orientation to @North@ (90 degrees). No line is drawn moving the turtle.
home :: TurtleCommand ()
home = TurtleCommand $ \ turtle -> do
  let ts = turtLens_ turtle
  ts . T.position       .= (0, 0)
  ts . T.heading        .= 90

-- | Warps the turtle to a new position.
--   The turtle jumps to this new position with no animation. If the pen is down
--   then a line is drawn.
--   
--   This does not affect the turtle's heading.
goto :: P.Point -- ^ Position to warp to.
     -> TurtleCommand ()
goto point = TurtleCommand $ \ turtle -> do
  t <- tData_ turtle
  let startP = t ^. T.position
  when (t ^. T.penDown) $ addPicture 
                        $ color (t ^. T.penColor) 
                        $ thickLine startP point (t ^. T.penSize)
  turtLens_ turtle . T.position .= point

-- | Alias of `goto`.
setPosition :: P.Point -> TurtleCommand ()
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
heading :: TurtleCommand Float -- ^ Returned heading as angle in degrees.
heading = getter_ 0 T.heading

-- | Sets the turtle's heading. See `heading`.
setHeading :: Float -- ^ Heading to apply. 
           -> TurtleCommand ()
setHeading = setter_ T.heading

-- | Returns the turtle's pen color.
--   The color of the turtle's pen.The default color is @black@.
penColor :: TurtleCommand Color -- ^ Returned current pen color.
penColor = getter_ black T.penColor

-- | Set the turtle's pen color.
--  See `penColor`.
setPenColor :: Color -- ^ New pen color to apply
            -> TurtleCommand ()
setPenColor = setter_ T.penColor

-- | Returns whether the turtle's pen is down.
--   When the turtle's pen is down it will draw a line when it moves.
--   The default value is @True@.
penDown :: TurtleCommand Bool -- ^ True if pen is down, false if not.
penDown = getter_ False T.penDown

-- | Sets the turtle's pen to down or up.
--   See `penDown`.
setPenDown :: Bool -- ^ New state for pen flag. True for down. False for up.
           -> TurtleCommand ()
setPenDown = setter_ T.penDown

-- | Returns the turtle's pen size.
--   Defaults to @2@.
penSize :: TurtleCommand Float -- ^ Size of turtle's pen.
penSize = getter_ 0 T.penSize

-- | Sets the turtle's pen size.
--   See `penSize`.
setPenSize :: Float -- ^ New size for turtle's pen.
           -> TurtleCommand ()
setPenSize = setter_ T.penSize

-- | Returns whether the turtle is visible.
--   The default value is @True@.
visible :: TurtleCommand Bool -- ^ @True@ if turtle is visible, @False@ if not.
visible = getter_ False T.visible

-- | Sets the turtle's visibility.
--   See `visible`.
setVisible :: Bool -- ^ New state for visible flag.
           -> TurtleCommand ()
setVisible = setter_ T.visible

-- | Returns the turtle's current speed.
--   Speed is is @distance@ per second.
--   A speed of @0 is equivalent to no animation being performed and instant 
--   movement.
-- The default value is @200@.
speed :: TurtleCommand Float -- ^ Speed of turtle.
speed = getter_ 0 T.speed

-- | Sets the turtle's speed.
--   See `speed`.
setSpeed :: Float -- ^ New speed.
         -> TurtleCommand ()
setSpeed = setter_ T.speed

-- | Returns the turtle's current rotation speed.
--   Rotation speed is is the speed in seconds it takes to do a full revolution.
--   A speed of @0@ is equivalent to no animation being performed and instant 
--   rotation.
-- The default value is @20@.
rotationSpeed :: TurtleCommand Float -- ^ Rotation speed of turtle.
rotationSpeed = getter_ 0 T.rotationSpeed

-- | Sets the turtle's rotation speed.
--   See `rotationSpeed`.
setRotationSpeed :: Float -- ^ New rotation speed.
                 -> TurtleCommand ()
setRotationSpeed = setter_ T.rotationSpeed

-- | Gets the turtle's representation as a `Picture`.
representation :: TurtleCommand Picture
representation = getter_ blank T.representation

{- | Sets the turtle's representation to a `Picture`.
   See `representation`.
   For example, to set the turtle as a red circle:
   
   @
    import Graphics.WorldTurtle
    import qualified Graphics.Gloss.Data.Picture as G

    myCommand :: TurtleCommand ()
    myCommand = do
      setPenColor red
      setRepresentation (G.color red $ G.circleSolid 10)
      forward 90
   @
-}
setRepresentation :: Picture -- ^ Picture to apply.
                  -> TurtleCommand ()
setRepresentation = setter_ T.representation

-- | Clears all drawings form the canvas. Does not alter any turtle's state.
clear :: WorldCommand ()
clear = WorldCommand $ pics .= []

-- | Sleep for a given amount of time in seconds. When sleeping no animation 
--   runs. A negative value will be clamped to @0@.
sleep :: Float -> WorldCommand ()
sleep = WorldCommand . decrementSimTime . max 0

-- | Given a command, runs the command, then resets the turtle's state back to
--   what the state was before the command was run.
branch :: TurtleCommand a -> TurtleCommand a
branch (TurtleCommand p ) = TurtleCommand $ \ turtle -> do
  t <- tData_ turtle
  output <- p turtle
  turtLens_ turtle .= t
  return output

-- | @90@ degrees.
north :: Float
north = 90

-- | @0@ degrees.
east :: Float
east = 0

-- | @180@ degrees.
west :: Float 
west = 180

-- | @270@ degrees.
south :: Float
south = 270

{-
   Here be dirty helper functions:
-}

-- | Looks up the turtle data for the given turtle in the state monad.
-- This type signature comes form GHC...my prism-foo is not good enough to sugar it.
turtLens_ :: Applicative f 
          => Turtle 
          -> (T.TurtleData -> f T.TurtleData) 
          -> TSC b 
          -> f (TSC b) 
turtLens_ t = turtles . at t . _Just
{-# INLINE turtLens_ #-}

-- | This is a helper function for our getter commands.
--   It takes a default value, the lense to compose, and the turtle to inspect.
getter_ :: a -> Lens' T.TurtleData a -> TurtleCommand a
getter_ def l = 
  TurtleCommand $ \ t -> fromMaybe def <$> preuse (turtLens_ t . l)
{-# INLINE getter_ #-}

-- | This is a helper function that extracts the turtle data for a given turtle.
tData_ :: Turtle -> SeqC T.TurtleData
tData_ = seqT $ getter_ T.defaultTurtle id
{-# INLINE tData_ #-}

-- | This is a helper function for our setter commands
-- It takes a lens, the value to apply, and the turtle to modify.
setter_ :: Lens' T.TurtleData b -> b -> TurtleCommand ()
setter_ l val = 
  TurtleCommand $ \ t -> turtLens_ t . l .= val
{-# INLINE setter_ #-}
