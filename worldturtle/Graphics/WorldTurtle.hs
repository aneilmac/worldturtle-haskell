{-|
Module      : Graphics.WorldTurtle
Description : WorldTurtle
Copyright   : (c) Archibald Neil MacDonald, 2020
License     : BSD3
Maintainer  : FortOyer@hotmail.co.uk
Stability   : experimental
Portability : POSIX

"Graphics.WorldTurtle" is a module for writing and rendering turtle graphics
in Haskell.

Take a look at the
     [examples](https://github.com/FortOyer/worldturtle-haskell#examples) on
Github!

-}
module Graphics.WorldTurtle
     ( 
     -- * Running the world
     -- $running
       WorldCommand
     , runWorld
     , runTurtle
     , TurtleCommand 
     , run 
     , (>/>)
     -- * Parallel animation
     -- $parallel
     , (<|>)
     -- * Further documentation
     , module Graphics.WorldTurtle.Commands
     , module Graphics.WorldTurtle.Shapes
     , module Graphics.WorldTurtle.Color
     ) where

import Control.Applicative ((<|>))

import Graphics.Gloss.Data.Display (Display (..))
import qualified Graphics.Gloss.Data.ViewState as G
import qualified Graphics.Gloss.Data.ViewPort as G
import qualified Graphics.Gloss.Interface.Pure.Game as G

import Graphics.WorldTurtle.Color
import Graphics.WorldTurtle.Commands
import Graphics.WorldTurtle.Internal.Sequence (renderTurtle)
import Graphics.WorldTurtle.Internal.Commands (TurtleCommand, seqT
                                              , WorldCommand (..), seqW)
import Graphics.WorldTurtle.Shapes

-- | `run` takes a `Turtle`, and a `TurtleCommand`. The command is applied
--   to the turtle, and the result of the computation is returned as a 
--   `WorldCommand`!
--
--  For example, to get a turtle's @(x, y)@ `position` one might write:
--
--  @
--    myCommand :: WorldCommand ()
--    myCommand = do
--      t <- makeTurtle
--      (x, y) <- run t position
--  @
--
-- Or to draw a right angle:
--
--  @
--    myCommand :: WorldCommand ()
--    myCommand = do
--      t <- makeTurtle
--      run t $ forward 10 >> right 90 >> forward 10
--  @
--
run :: Turtle 
    -> TurtleCommand a 
    -> WorldCommand a
run t commands = WorldCommand $ seqT commands t

-- | Infix version of `run`.
--   This is a convenience function.
-- 
--   To draw a right angle:
--
--  @
--    myCommand :: WorldCommand ()
--    myCommand = do
--      t <- makeTurtle
--      t >/> do 
--        forward 10
--        right 90
--        forward 10
--  @
--
(>/>) :: Turtle -> TurtleCommand a -> WorldCommand a
(>/>) = run
infixl 1 >/>

data World = World { elapsedTime :: !Float
                   , running :: !Bool
                   , state :: !G.ViewState 
                   }

{- | `runWorld` takes a `WorldCommand` and produces the animation in a new
     window! 

     The simplest way to run `runWorld` is to execute it directly from 
     your main function like so:

     @
         main :: IO ()
         main = runWorld yourOwnCoolCommand
     @

     While running, you can interact with the window in the following way:

     +------------------------------------------+-------------------+
     | Action                                   | Interaction       |
     +==========================================+===================+
     | Pan the viewport.                        | Click and drag    |
     +------------------------------------------+-------------------+
     | Zoom in/out.                             |Mousewheel up/down |
     +------------------------------------------+-------------------+
     | Reset the viewport to initial position.  | Spacebar          |
     +------------------------------------------+-------------------+
     | Reset the animation.                     | @R@ key           |
     +------------------------------------------+-------------------+
     | Pause the animation.                     | @P@ key           |
     +------------------------------------------+-------------------+
     | Quit                                     | Escape key        |
     +------------------------------------------+-------------------+
-}
runWorld :: WorldCommand () -- ^ Command sequence to execute
          -> IO ()
runWorld tc = G.play display white 30 defaultWorld iterateRender input timePass
  where display = InWindow "World Turtle" (800, 600) (400, 300)
        iterateRender w = G.applyViewPortToPicture 
                               (G.viewStateViewPort $ state w)
                        $! renderTurtle (seqW tc) (elapsedTime w)
        input e w 
             -- Reset key resets sim state (including unpausing). We 
             -- deliberately keep view state the same.
             | isResetKey_ e = w { elapsedTime = 0, running = True }
             -- Pause prevents any proceeding.
             | isPauseKey_ e = w { running = not $ running w }
             -- Let Gloss consume the command.
             | otherwise = w { state = G.updateViewStateWithEvent e $ state w } 
        -- Increment simulation time if we are not paused.
        timePass f w
         | running w = w { elapsedTime = f + elapsedTime w }
         | otherwise = w

defaultWorld :: World
defaultWorld = World 0 True 
             $ G.viewStateInitWithConfig 
             -- Easier to do this to have spacebar overwrite R.
             $ reverse 
             $ (G.CRestore, [(G.SpecialKey G.KeySpace, Nothing)])
             : G.defaultCommandConfig

-- | Tests to see if a key-event is the reset key.
isResetKey_ :: G.Event -> Bool
isResetKey_ (G.EventKey (G.Char 'r') G.Down _ _)  = True
isResetKey_ (G.EventKey (G.Char 'R') G.Down _ _)  = True
isResetKey_ _ = False

-- Tests to see if a key event is the pause key
isPauseKey_ :: G.Event -> Bool
isPauseKey_ (G.EventKey (G.Char 'p') G.Down _ _)  = True
isPauseKey_ (G.EventKey (G.Char 'P') G.Down _ _)  = True
isPauseKey_ _ = False

-- | This is a convenient simplification of `runWorld` where we implicitly
--   create a single turtle and execute this.
runTurtle :: TurtleCommand () -> IO ()
runTurtle c = runWorld $ makeTurtle >>= \ t -> run t c

{- $running

It is easy to create and animate a turtle. You just pass your commands to
`runWorld` like so:

@
     import Control.Monad (replicateM_)
     import Graphics.WorldTurtle

     myCommand :: WorldCommand ()
     myCommand = do 
       t <- makeTurtle
       t >/> replicateM_ 4 (forward 90 t >> right 90 t)

     main :: IO ()
     main = runWorld myCommand
@

Which will produce this animation

![basic_turtle_square gif](docs/images/basic_turtle_square.gif)

If you only need to animate a single turtle, you can use `runTurtle` instead
for convenience. This is equivalent:

@
     import Control.Monad (replicateM_)
     import Graphics.WorldTurtle

     myCommand :: TurtleCommand ()
     myCommand = replicateM_ 4 $ do
          forward 90 t
          right 90 t

     main :: IO ()
     main = runTurtle myCommand
@

-}

{- $parallel

   We already know that `WorldCommand`s can be combined with `(>>)`, but the
   alternative operation `(<|>)` can alo be used to combine two 
   `WorldCommand`s. This has a special meaning: do both animations at the 
   same time!

   ![parallel and serial gif](docs/images/parallel_serial_turtles.gif)

   Note that the result of @a \<|\> b@ is:
   
   >>> a <|> b
   a

   when /a/ is not `Control.Monad.mzero`.
-}