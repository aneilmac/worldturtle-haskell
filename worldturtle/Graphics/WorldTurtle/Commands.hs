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

This module is a collection of all the commands used to manipulate a turtle!

-}
module Graphics.WorldTurtle.Commands
  (
  -- * Setting up a turtle.
    Turtle
  , makeTurtle
  , makeTurtle'
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
  , Graphics.WorldTurtle.Commands.circle
  , goto
  , setPosition
  , home
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
  , penSize
  , visible
  -- * Drawing state.
  , setPenColor
  , setPenDown
  , setPenSize
  , setRepresentation
  , setVisible
  -- * Canvas commands.
  , clear
  , sleep
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
    main = runTurtle $ do
      t <- makeTurtle
      forward 90 t
   @

The default turtle starts at position (0, 0) and is orientated @North@.

-}
makeTurtle :: TurtleCommand Turtle
makeTurtle = TurtleCommand generateTurtle

{-| This variant of `makeTurtle` takes a starting position, a starting 
    orientation, and a color to apply to the turtle and the turtle's pen.

    @
      myCommand :: TurtleCommand ()
      myCommand = do
        t1 <- makeTurtle' (0, 0)  0 green
        t2 <- makeTurtle' (0, 0) 90 red
        forward 90 t1 \<|\> forward 90 t2
    @

    See `makeTurtle`.
-}
makeTurtle' :: Point -- ^ Initial position of the turtle.
            -> Float -- ^ Initial heading of the turtle.
            -> Color -- ^ Color of the turtle and the turtle's pen.
            -> TurtleCommand Turtle -- ^ The generated turtle.
makeTurtle' p f c = TurtleCommand $ do 
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
         -> Turtle -- ^ The turtle to move.
         -> TurtleCommand ()
backward !d = forward (-d)

-- | Shorthand for `backward`.
bk :: Float -> Turtle -> TurtleCommand ()
bk = backward

-- | Turn a turtle right by the given degrees amount.
right :: Float -- ^ Rotation amount to apply to turtle.
      -> Turtle -- ^ The turtle to rotate.
      -> TurtleCommand ()
right !r = left (-r)

-- | Shorthand for `right`.
rt :: Float -> Turtle -> TurtleCommand ()
rt = right

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
        -> Turtle -- ^ The turtle to move.
        -> TurtleCommand ()
forward !d turtle = TurtleCommand $ do
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
fd :: Float -> Turtle -> TurtleCommand ()
fd = forward

-- | Stamp a copy of the turtle shape onto the canvas at the current turtle 
--   position.
stamp :: Turtle -- ^ The turtle with the shape to be copied.
      -> TurtleCommand ()
stamp turtle = TurtleCommand $ tData_ turtle >>= addPicture . T.drawTurtle 

-- | Turn a turtle left by the given degrees amount.
left :: Float -- ^ Rotation amount to apply to turtle.
     -> Turtle -- ^ The turtle to rotate.
     -> TurtleCommand ()
left !r turtle = TurtleCommand $ do
    t <- tData_ turtle
    let r' = P.normalizeDirection r
    animate' (P.degToRad r') (t ^. T.speed) $ \q -> do
      let !h = t ^. T.heading
      --let q' = if r > 0 then q else -q
      let !newHeading = P.normalizeHeading $ h + q * r'
      --  Get new heading via percentage
      turtLens_ turtle . T.heading .= newHeading

-- | Shorthand for `left`.
lt :: Float -> Turtle -> TurtleCommand ()
lt = left

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
circle  :: Float -- ^ Radius of the circle.
        -> Float -- ^ Angle to travel in degrees. 
                 -- For example: @360@ for a full circle or @180@ for a 
                 -- semicircle.
        -> Turtle -- ^ Turtle to move in a circle.
        -> TurtleCommand ()
circle !radius !r turtle = TurtleCommand $ do
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
position :: Turtle -- ^ Turtle to query.
         -> TurtleCommand P.Point -- ^ Returned current point.
position = getter_ (0, 0) T.position

-- | Warps the turtle to its starting position @(0, 0)@ and resets the
-- orientation to @North@ (90 degrees). No line is drawn moving the turtle.
home :: Turtle
     -> TurtleCommand ()
home turtle = TurtleCommand $ do
  let ts = turtLens_ turtle
  ts . T.position       .= (0, 0)
  ts . T.heading        .= 90

-- | Warps the turtle to a new position.
--   The turtle jumps to this new position with no animation. If the pen is down
--   then a line is drawn.
goto :: P.Point -- ^ Position to warp to.
     -> Turtle -- ^ Turtle to modify.
     -> TurtleCommand ()
goto point turtle = TurtleCommand $ do
  t <- tData_ turtle
  let startP = t ^. T.position
  when (t ^. T.penDown) $ addPicture 
                        $ color (t ^. T.penColor) 
                        $ thickLine startP point (t ^. T.penSize)
  turtLens_ turtle . T.position .= point

-- | Alias of `goto`.
setPosition :: P.Point -> Turtle -> TurtleCommand ()
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
setHeading :: Float -- ^ Heading to apply. 
           -> Turtle -- ^ Turtle to set.
           -> TurtleCommand ()
setHeading = setter_ T.heading

-- | Returns the turtle's pen color.
--   The color of the turtle's pen.The default color is @black@.
penColor :: Turtle -- ^ Turtle to query.
         -> TurtleCommand Color -- ^ Returned current pen color.
penColor = getter_ black T.penColor

-- | Set the turtle's pen color.
--  See `penColor`.
setPenColor :: Color -- ^ New pen color to apply
            -> Turtle -- ^ Turtle to modify.
            -> TurtleCommand ()
setPenColor = setter_ T.penColor

-- | Returns whether the turtle's pen is down.
--   When the turtle's pen is down it will draw a line when it moves.
--   The default value is @true@.
penDown :: Turtle -- ^ Turtle to query.
         -> TurtleCommand Bool -- ^ True if pen is down, false if not.
penDown = getter_ False T.penDown

-- | Sets the turtle's pen to down or up.
--   See `penDown`.
setPenDown :: Bool -- ^ New state for pen flag. True for down. False for up.
           -> Turtle -- ^ Turtle to modify.
           -> TurtleCommand ()
setPenDown = setter_ T.penDown

-- | Returns the turtle's pen size.
--   Defaults to @2@.
penSize :: Turtle -- ^ Turtle to query.
         -> TurtleCommand Float -- ^ Size of turtle's pen.
penSize = getter_ 0 T.penSize

-- | Sets the turtle's pen size.
--   See `penSize`.
setPenSize :: Float -- ^ New size for turtle's pen.
           -> Turtle -- ^ Turtle to modify.
           -> TurtleCommand ()
setPenSize = setter_ T.penSize

-- | Returns whether the turtle is visible.
--   The default value is @true@.
visible :: Turtle -- ^ Turtle to query.
        -> TurtleCommand Bool -- ^ True if turtle is visible,false if not.
visible = getter_ False T.visible

-- | Sets the turtle's visibility.
--   See `visible`.
setVisible :: Bool -- ^ New state for visible flag.
           -> Turtle -- ^ Turtle to modify.
           -> TurtleCommand ()
setVisible = setter_ T.visible

-- | Returns whether the turtle's current speed.
--   Speed is is @distance@ per second.
--   A speed of 0 is equivalent to no animation being performed and instant 
--   drawing.
-- The default value is @200@.
speed :: Turtle -- ^ Turtle to query.
      -> TurtleCommand Float
speed = getter_ 0 T.speed

-- | Sets the turtle's speed.
--   See `speed`.
setSpeed :: Float -- ^ New speed.
         -> Turtle -- ^ Turtle to modify.
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
    import Graphics.WorldTurtle
    import qualified Graphics.Gloss.Data.Picture as G

    myCommand :: TurtleCommand ()
    myCommand = do
      t <- makeTurtle
      setPenColor red t
      setRepresentation (G.color red $ G.circleSolid 10) t
      forward 90 t
   @
-}
setRepresentation :: Picture
                  -> Turtle -- ^ Turtle to mutate.
                  -> TurtleCommand ()
setRepresentation = setter_ T.representation

-- | Clears all drawings form the canvas. Does not alter any turtle's state.
clear :: TurtleCommand ()
clear = TurtleCommand $ pics .= []

-- | Sleep for a given amount of time in seconds. When sleeping no animation 
--   runs. A negative value will be clamped to @0@.
sleep :: Float -> TurtleCommand ()
sleep = TurtleCommand . decrementSimTime . max 0

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
getter_ :: a -> Lens' T.TurtleData a -> Turtle -> TurtleCommand a
getter_ def l t = 
  TurtleCommand $ fromMaybe def <$> preuse (turtLens_ t . l)
{-# INLINE getter_ #-}

-- | This is a helper function that extracts the turtle data for a given turtle.
tData_ :: Turtle -> SeqC T.TurtleData
tData_ = seqT <$> getter_ T.defaultTurtle id
{-# INLINE tData_ #-}

-- | This is a helper function for our setter commands
-- It takes a lens, the value to apply, and the turtle to modify.
setter_ :: Lens' T.TurtleData b -> b -> Turtle -> TurtleCommand ()
setter_ l val t = 
  TurtleCommand $ turtLens_ t . l .= val
{-# INLINE setter_ #-}
