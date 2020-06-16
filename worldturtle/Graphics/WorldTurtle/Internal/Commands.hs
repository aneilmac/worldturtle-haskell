{-# OPTIONS_HADDOCK hide #-}
module Graphics.WorldTurtle.Internal.Commands
  ( TurtleCommand (..)
  , WorldCommand (..)
  , run
  ) where

import Control.Applicative
import Control.Monad

import Graphics.Gloss.Data.Picture (text)

import Graphics.WorldTurtle.Internal.Sequence

{-| A `TurtleCommand` represents an instruction to execute on a turtle.
    It could be as simple as "draw a line" or more complicated like 
    "draw 300 circles."

    `TurtleCommand`s can be executed in order by combining them using
    the monadic operator `(>>)`.

    For example, to draw an equilateral triangle 
    using [do notation](https://en.wikibooks.org/wiki/Haskell/do_notation):

    > drawTriangle :: TurtleCommand ()
    > drawTriangle = do
    >   setHeading east
    >   forward 100
    >   left 120
    >   forward 100
    >   left 120
    >   forward 100

    Which would produce:

    ![draw triangle gif](docs/images/drawtriangle.gif)
-}
newtype TurtleCommand a = TurtleCommand 
  { 
    seqT :: Turtle -> SequenceCommand a
  }

instance Functor TurtleCommand where
  fmap f (TurtleCommand a) = TurtleCommand $ \ t -> fmap f (a t)

instance Applicative TurtleCommand where
  pure a = TurtleCommand $ \ _ -> pure a
  liftA2 f (TurtleCommand a) (TurtleCommand b) = 
    TurtleCommand $ \ t -> liftA2 f (a t) (b t)

instance Monad TurtleCommand where
  (TurtleCommand a) >>= f = TurtleCommand $ \ t -> a t >>= \s -> seqT (f s) t

instance MonadFail TurtleCommand where
  fail t = TurtleCommand $ \ _ -> do
    addPicture $ text t
    fail t

{- | A `WorldCommand` represents an instruction that affects the entire 
     animation canvas.
    
    This could be as simple as "make a turtle" or more complicated like 
    "run these 5 turtles in parallel."

    Like `TurtleCommand`s, `WorldCommand`s can be executed in order by 
    combining commands in order using the monadic operator `(>>)`.

    To execute a `TurtleCommand` in a `WorldCommand`, use either the 
    `Graphics.WorldTurtle.run` function or the 
    `Graphics.WorldTurtle.>/>` operator.

    For how to achieve parallel animations
    see "Graphics.WorldTurtle#parallel".
-}
newtype WorldCommand a = WorldCommand 
  { 
    seqW :: SequenceCommand a
  }

instance Functor WorldCommand where
  fmap f (WorldCommand a) = WorldCommand $ fmap f a

instance Applicative WorldCommand where
  pure a = WorldCommand $ pure a
  liftA2 f (WorldCommand a) (WorldCommand b) = WorldCommand $ liftA2 f a b

instance Monad WorldCommand where
  (WorldCommand a) >>= f = WorldCommand $ a >>= \s -> seqW (f s)

instance Alternative WorldCommand where
  empty = WorldCommand empty
  (<|>) (WorldCommand a) (WorldCommand b) = WorldCommand $ alternateSequence a b

instance Semigroup a => Semigroup (WorldCommand a) where
  (WorldCommand a) <> (WorldCommand b) = WorldCommand $ combineSequence a b

instance MonadPlus WorldCommand

instance MonadFail WorldCommand where
  fail t = WorldCommand $ do
    addPicture $ text t
    fail t

-- | `run` takes a `TurtleCommand` and a `Turtle` to execute the command on. 
--  The result of the computation is returned wrapped in a `WorldCommand`.
--
--  For example, to create  a turtle and get its @x@ `position` one might 
--  write:
--
--  >  myCommand :: Turtle -> WorldCommand Float
--  >  myCommand t = do
--  >    (x, _) <- run position t
--  >    return x
--
--  Or to create a command that accepts a turtle and draws a right angle:
--
--  > myCommand :: Turtle -> WorldCommand ()
--  > myCommand = run $ forward 10 >> right 90 >> forward 10
run :: TurtleCommand a -- ^ Command to execute
    -> Turtle -- ^ Turtle to apply the command upon.
    -> WorldCommand a -- ^ Result as a `WorldCommand`
run c = WorldCommand . seqT c
