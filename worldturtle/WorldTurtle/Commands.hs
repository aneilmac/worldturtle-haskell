{-# LANGUAGE RankNTypes #-}
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
  , WorldTurtle.Commands.circle
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
  -- * Common constants
  , east
  , north
  , west
  , south
  ) where

import WorldTurtle.Shapes

import WorldTurtle.Internal.Commands
import qualified WorldTurtle.Internal.Turtle as T
import qualified WorldTurtle.Internal.Coords as P

import Graphics.Gloss.Data.Color (Color, white, black)
import Graphics.Gloss.Data.Display (Display (..))
import qualified Graphics.Gloss.Interface.Pure.Animate as G (animate)
import Graphics.Gloss.Data.Picture

import Control.Lens
import Control.Monad
import Control.Applicative

import Data.Maybe (fromMaybe)

-- $executionDoc
-- A


type SeqC a = SequenceCommand (AlmostVal ()) a

newtype TurtleCommand a = TurtleCommand 
  { 
    seqT :: SeqC a
  }

instance Functor TurtleCommand where
  fmap f (TurtleCommand a) = TurtleCommand $ fmap f a

instance Applicative TurtleCommand where
  pure a = TurtleCommand $ pure a
  liftA2 f (TurtleCommand a) (TurtleCommand b) = TurtleCommand $ liftA2 f a b

instance Monad TurtleCommand where
  (TurtleCommand a) >>= f = TurtleCommand $ a >>= \s -> seqT (f s)

instance Alternative TurtleCommand where
  empty = TurtleCommand failSequence
  (<|>) (TurtleCommand a) (TurtleCommand b) = 
    TurtleCommand $ alternateSequence a b

instance Semigroup a => Semigroup (TurtleCommand a) where
  (TurtleCommand a) <> (TurtleCommand b) = 
    TurtleCommand $ combineSequence a b
    
instance MonadPlus TurtleCommand

instance MonadFail TurtleCommand where
  fail t = TurtleCommand $ do
    addPicture $ text t
    failSequence

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

-- | This is the heart of the turtle system. When run, this command will draw
-- canvas and start animating the given `TurtleCommand`.
runTurtle :: TurtleCommand () -- ^ Command sequence to execute
          -> IO ()
runTurtle tc = G.animate display white iterateRender
     where display = InWindow "World Turtle" (800, 600) (400, 300)
           iterateRender f = renderTurtle (seqT tc) f

-- | Move the turtle backward by the specified @distance@, in the direction the 
--   turtle is headed.
backward :: Float -- ^ Distance to move the turtle.
         -> Turtle -- ^ The turtle to move.
         -> TurtleCommand ()
backward d = forward (-d)

-- | Shorthand for `backward`.
bk :: Float -> Turtle -> TurtleCommand ()
bk = backward

-- | Turn a turtle right by the given degrees amount.
right :: Float -- ^ Rotation amount to apply to turtle.
      -> Turtle -- ^ The turtle to rotate.
      -> TurtleCommand ()
right r = left (-r)

-- | Shorthand for `right`.
rt :: Float -> Turtle -> TurtleCommand ()
rt = right

-- | Move the turtle forward by the specified @distance@, in the direction the 
--   turtle is headed.
forward :: Float -- ^ Distance to move the turtle.
        -> Turtle -- ^ The turtle to move.
        -> TurtleCommand ()
forward d turtle = TurtleCommand $ do
    t <- tData_ turtle
    --  Get origin point
    animate' d (t ^. T.speed) $ \ q -> do
      let startP = t ^. T.position
      let vec = P.rotateV (P.degToRad $ t ^. T.heading) (d, 0)
      let endP = vec P.+ startP
      let midP = P.lerp q startP endP
      --  Get new endpoint via percentage
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
left r turtle = TurtleCommand $ do
    t <- tData_ turtle
    let r' = P.normalizeDirection r
    animate' (P.degToRad r') (t ^. T.speed) $ \q -> do
      let h = t ^. T.heading
      --let q' = if r > 0 then q else -q
      let newHeading = P.normalizeHeading $ h + q * r'
      --  Get new heading via percentage
      turtLens_ turtle . T.heading .= newHeading

-- | Shorthand for `left`.
lt :: Float -> Turtle -> TurtleCommand ()
lt = left

circle  :: Float -- ^ Radius of the circle.
        -> Float -- ^ Angle to travel in degrees. 
                 -- For example: @360@ for a full circle or @180@ for a 
                 -- semicircle.
        -> Turtle -- ^ Turtle to move in a circle.
        -> TurtleCommand ()
circle radius r turtle = TurtleCommand $ do
  t <- tData_ turtle
  let r' = P.normalizeHeading r
  animate' (radius * P.degToRad r') (t ^. T.speed) $ \q -> do
    let startAngle = t ^. T.heading + 90
    let p = t ^. T.position
    let angle = r' * q
    when (t ^. T.penDown) $ do -- don't draw if pen isn't in down state
      let lPic  = translate (fst p) (snd p)
                $ rotate (180 - startAngle)
                $ translate (-radius) 0
                $ color (t ^. T.penColor)
                $ rotate (if radius >= 0 then 0 else 180)
                $ thickArc 0 (angle) radius (t ^. T.penSize)
      addPicture lPic

    let s = P.degToRad $ startAngle
    let a = P.degToRad $ angle + startAngle
    let cS = cos s
    let sS = sin s
    let cA = cos a
    let sA = sin a
    let rx = fst p - (radius * (cA - cS))
    let ry = snd p - (radius * (sA - sS))

    -- Update the turtle with the new values.
    let ts = turtLens_ turtle
    ts . T.heading .= (P.normalizeHeading $ startAngle - 90 + angle)
    ts . T.position .= (rx , ry)

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
--   See `peSize`.
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
-- The default value is @TODO - DECIDE ON A DEFAULT VALUE@.
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
    import WorldTurtle
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

-- | This is a helper function for our getter commands.
--   It takes a default value, the lense to compose, and the turtle to inspect.
getter_ :: a -> Lens' T.TurtleData a -> Turtle -> TurtleCommand a
getter_ def l t = 
  TurtleCommand $ fromMaybe def <$> preuse (turtLens_ t . l)

-- | This is a helper function that extracts the turtle data for a given turtle.
tData_ :: Turtle -> SeqC T.TurtleData
tData_ = seqT <$> getter_ T.defaultTurtle id

-- | This is a helper function for our setter commands
-- It takes a lens, the value to apply, and the turtle to modify.
setter_ :: Lens' T.TurtleData b -> b -> Turtle -> TurtleCommand ()
setter_ l val t = 
  TurtleCommand $ turtLens_ t . l .= val