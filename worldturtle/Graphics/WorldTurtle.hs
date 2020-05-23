{-|
Module      : Graphics.WorldTurtle
Description : WorldTurtle
Copyright   : (c) Archibald Neil MacDonald, 2020
License     : BSD3
Maintainer  : FortOyer@hotmail.co.uk
Stability   : experimental
Portability : POSIX

This module exposes the Turtle running commands.

-}
module Graphics.WorldTurtle
     ( 
     -- * Running the turtle
       TurtleCommand 
     , runTurtle 
     -- * Parallel animation
     , (<|>)
     -- * Modules
     , module Graphics.WorldTurtle.Commands
     , module Graphics.WorldTurtle.Shapes
     , module Graphics.Gloss.Data.Color
     ) where

import Control.Applicative ((<|>))

import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Display (Display (..))
import qualified Graphics.Gloss.Data.ViewState as G
import qualified Graphics.Gloss.Data.ViewPort as G
import qualified Graphics.Gloss.Interface.Pure.Game as G

import Graphics.WorldTurtle.Commands
import Graphics.WorldTurtle.Internal.Sequence (renderTurtle)
import Graphics.WorldTurtle.Internal.Commands (TurtleCommand, seqT)
import Graphics.WorldTurtle.Shapes

data World = World { elapsedTime :: !Float
                   , running :: !Bool
                   , state :: !G.ViewState 
                   }

runTurtle :: TurtleCommand () -- ^ Command sequence to execute
          -> IO ()
runTurtle tc = G.play display white 30 defaultWorld iterateRender input timePass
  where display = InWindow "World Turtle" (800, 600) (400, 300)
        iterateRender w = G.applyViewPortToPicture 
                               (G.viewStateViewPort $ state w)
                        $! renderTurtle (seqT tc) (elapsedTime w)
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