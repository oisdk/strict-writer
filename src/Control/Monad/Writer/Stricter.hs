{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE ViewPatterns               #-}

module Control.Monad.Writer.Stricter
  (WriterT
  ,runWriterT
  ,pattern WriterT
  ,Weighted
  ,runWeighted
  ,pattern Weighted
  ,execWriterT
  ,evalWriterT
  ,execWeighted
  ,evalWeighted)
  where

import           Control.Applicative
import           Control.Monad.Identity
import           Control.Monad.State.Strict
import           Control.Monad.Writer hiding (WriterT(..), execWriterT)
import           Data.Coerce

-- | A monad transformer similar to 'Control.Monad.Writer.Strict.WriterT', except
-- that it does not leak space.
newtype WriterT s m a =
    WriterT_ (StateT s m a)
    deriving (Functor,Applicative,Monad,MonadTrans)

runWriterT
    :: Monoid s
    => WriterT s m a -> m (a, s)
runWriterT =
    (coerce :: (StateT s m a -> m (a, s)) -> WriterT s m a -> m (a, s))
        (`runStateT` mempty)
{-# INLINE runWriterT #-}

pattern WriterT :: (Functor m, Monoid s) => m (a, s) -> WriterT s m a
pattern WriterT x <- (runWriterT -> x) where
  WriterT y = WriterT_ (StateT (\s -> (fmap.fmap) (mappend s) y))

type Weighted s = WriterT s Identity

pattern Weighted :: Monoid s => (a, s) -> Weighted s a
pattern Weighted x <- (runWeighted -> x) where
  Weighted (y,p) = WriterT_ (StateT (\s -> Identity (y, mappend p s)))

runWeighted
    :: Monoid s
    => Weighted s a -> (a, s)
runWeighted =
    (coerce :: (WriterT s Identity a -> Identity (a, s)) -> (WriterT s Identity a -> (a, s)))
        runWriterT
{-# INLINE runWeighted #-}

instance (Monoid s, Monad m) => MonadWriter s (WriterT s m) where
  writer (x, s) = WriterT (pure (x, s))
  {-# INLINE writer #-}
  listen (WriterT_ s) = WriterT_ ((,) <$> s <*> get)
  {-# INLINE listen #-}
  pass (WriterT_ s) = WriterT_ (passS s) where
    passS = (=<<) (uncurry (<$) . fmap (modify . coerce))
  {-# INLINE pass #-}

evalWriterT :: (Monad m, Monoid s) => WriterT s m a -> m a
evalWriterT =
    (coerce :: (StateT s m a -> m a) -> WriterT s m a -> m a)
        (`evalStateT` mempty)
{-# INLINE evalWriterT #-}

execWriterT :: (Monad m, Monoid s) => WriterT s m a -> m s
execWriterT =
    (coerce :: (StateT s m a -> m s) -> WriterT s m a -> m s)
        (`execStateT` mempty)
{-# INLINE execWriterT #-}

evalWeighted :: Monoid s => Weighted s a -> a
evalWeighted =
    (coerce :: (State s a -> a) -> Weighted s a -> a)
        (`evalState` mempty)
{-# INLINE evalWeighted #-}

execWeighted :: Monoid s => Weighted s a -> s
execWeighted =
    (coerce :: (State s a -> s) -> Weighted s a -> s)
        (`execState` mempty)
{-# INLINE execWeighted #-}

instance (Alternative m, Monad m, Monoid s) => Alternative (WriterT s m) where
  empty = WriterT empty
  {-# INLINE empty #-}
  WriterT x <|> WriterT y = WriterT (x <|> y)
  _ <|> _ = undefined
  {-# INLINE (<|>) #-}
