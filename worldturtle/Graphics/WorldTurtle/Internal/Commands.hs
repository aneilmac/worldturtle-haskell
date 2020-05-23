{-# OPTIONS_HADDOCK hide #-}
module Graphics.WorldTurtle.Internal.Commands
  ( SeqC
  , TurtleCommand (..)
  ) where

import Control.Applicative
import Control.Monad

import Graphics.Gloss.Data.Picture (text)

import Graphics.WorldTurtle.Internal.Sequence

type SeqC a = SequenceCommand (AlmostVal ()) a

{-| A `TurtleCommand` represents an instruction to execute. It could be as
    simple as "draw a line" or more complicated like "draw 300 circles."
    
    `TurtleCommand`s can be executed in order by combining them using
    the monadic operator `(>>)`.

    Here is an example of how to write a function that when given a
    @size@ and a @turtle@, will return a new `TurtleCommand` which
    will draw a square with a length and breadth of @size@ using @turtle@.

   @
      drawSquare :: Float -> Turtle -> TurtleCommand ()
      drawSquare size t = replicateM_ 4 $ forward size t >> right 90 t
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
-}
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