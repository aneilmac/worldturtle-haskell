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