{-# OPTIONS_HADDOCK hide #-}
module Graphics.WorldTurtle.Internal.Commands
  ( SeqC
  , TurtleCommand (..)
  , WorldCommand (..)
  ) where

import Control.Applicative
import Control.Monad

import Graphics.Gloss.Data.Picture (text)

import Graphics.WorldTurtle.Internal.Sequence

type SeqC a = SequenceCommand (AlmostVal ()) a

{-| A `TurtleCommand` represents an instruction to execute on a specific turtle.
    It could be as simple as "draw a line" or more complicated like 
    "draw 300 circles."

    `TurtleCommand`s can be executed in order by combining them using
    the monadic operator `(>>)`.

    Here is an example of how to write a function that when given a
    @size@,  will return a new `TurtleCommand` which
    will draw a square with a length and breadth of @size@.

   @
      drawSquare :: Float -> TurtleCommand ()
      drawSquare size = replicateM_ 4 $ forward size >> right 90
   @

   This draws a square by doing the following in order:
   
   [@(1/4)@]: 

          * Move forward by @size@ amount. 

          * Turn right by @90@ degrees

     [@(2/4)@]:

          * Move forward by @size@ amount. 

          * Turn right by @90@ degrees

     [@(3/4)@]:

          * Move forward by @size@ amount. 

          * Turn right by @90@ degrees

     [@(4/4)@]:

          * Move forward by @size@ amount. 

          * Turn right by @90@ degrees

    Execute `drawSquare` on a turtle using `Graphics.WorldTurtle.(>/>)` function
    like so:

    @
      main = runWorld $ do
        t <- makeTurtle
        t >/> drawSquare 90
    @
-}
newtype TurtleCommand a = TurtleCommand 
  { 
    seqT :: Turtle -> SeqC a
  }

instance Functor TurtleCommand where
  fmap f (TurtleCommand a) = TurtleCommand $ \ t -> fmap f (a t)

instance Applicative TurtleCommand where
  pure a = TurtleCommand $ \ _ -> pure a
  liftA2 f (TurtleCommand a) (TurtleCommand b) = 
    TurtleCommand $ \ t -> liftA2 f (a t) (b t)

instance Monad TurtleCommand where
  (TurtleCommand a) >>= f = TurtleCommand $ \ t -> a t >>= \s -> seqT (f s) t

instance Alternative TurtleCommand where
  empty = TurtleCommand $ const failSequence
  (<|>) (TurtleCommand a) (TurtleCommand b) = TurtleCommand $ \ t -> 
    alternateSequence (a t) (b t)

instance Semigroup a => Semigroup (TurtleCommand a) where
  (TurtleCommand a) <> (TurtleCommand b) = TurtleCommand $ \ t -> 
    combineSequence (a t) (b t)

instance MonadPlus TurtleCommand

instance MonadFail TurtleCommand where
  fail t = TurtleCommand $ \ _ -> do
    addPicture $ text t
    failSequence

{- | A `WorldCommand` represents an instruction to execute in the world.
    It could be as simple as "make a turtle" or more complicated like 
    "run these 5 turtles in parallel."

    `WorldCommand`s can be executed in order by combining them using
    the monadic operator `(>>)`.
-}
newtype WorldCommand a = WorldCommand 
  { 
    seqW :: SeqC a
  }

instance Functor WorldCommand where
  fmap f (WorldCommand a) = WorldCommand $ fmap f a

instance Applicative WorldCommand where
  pure a = WorldCommand $ pure a
  liftA2 f (WorldCommand a) (WorldCommand b) = WorldCommand $ liftA2 f a b

instance Monad WorldCommand where
  (WorldCommand a) >>= f = WorldCommand $ a >>= \s -> seqW (f s)

instance Alternative WorldCommand where
  empty = WorldCommand failSequence
  (<|>) (WorldCommand a) (WorldCommand b) = WorldCommand $ alternateSequence a b

instance Semigroup a => Semigroup (WorldCommand a) where
  (WorldCommand a) <> (WorldCommand b) = WorldCommand $ combineSequence a b

instance MonadPlus WorldCommand

instance MonadFail WorldCommand where
  fail t = WorldCommand $ do
    addPicture $ text t
    failSequence
