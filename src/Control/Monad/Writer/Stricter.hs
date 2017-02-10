{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE Strict                     #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}

-- | This module provides monad transformer similar to
-- 'Control.Monad.Writer.Strict.WriterT', implemented using 'StateT', making it
-- tail recursive. (The traditional writer always leaks space: see
-- <https://mail.haskell.org/pipermail/libraries/2013-March/019528.html here>
-- for more information).
--
-- <https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#pattern-synonyms Pattern Synonyms>
-- are used to provide the same interface as
-- 'Control.Monad.Writer.Strict.WriterT'. Unfortunately, current GHC warns
-- whenever these patterns are used that there are unmatched patterns: the
-- <https://ghc.haskell.org/trac/ghc/ticket/8779 COMPLETE> pragma should solve
-- this problem in future version of GHC.
--
-- A pattern synonym is also provided for a non-transformer version of writer.
-- Again, this is just 'StateT' underneath, but its interface looks as if it was
-- defined like so:
--
-- > newtype Writer w a = Writer { runWriter :: (a, w) }

module Control.Monad.Writer.Stricter
  (
   -- * Transformer
   WriterT
  ,runWriterT
  ,pattern WriterT
  ,execWriterT
  ,evalWriterT
  ,
   -- * Plain
   Writer
  ,runWriter
  ,pattern Writer
  ,execWriter
  ,evalWriter)
  where

import           Control.Applicative
import           Control.Monad.Identity
import           Control.Monad.State.Strict

import           Control.Monad.Cont.Class
import           Control.Monad.Error.Class
import           Control.Monad.Fail
import           Control.Monad.Reader.Class
import           Control.Monad.Writer.Class

import           Data.Coerce
import           Data.Functor.Classes

import           Data.Monoid

-- | A monad transformer similar to 'Control.Monad.Writer.Strict.WriterT', except
-- that it does not leak space. It is implemented using a state monad, so that
-- `mappend` is tail recursive. See
-- <https://mail.haskell.org/pipermail/libraries/2013-March/019528.html this>
-- email to the Haskell libraries committee for more information.
--
-- Wherever possible, coercions are used to eliminate any overhead from the
-- newtype wrapper.
newtype WriterT s m a =
    WriterT_ (StateT s m a)
    deriving (Functor,Applicative,Monad,MonadTrans,MonadCont,MonadError e
             ,MonadReader r,MonadFix,MonadFail,MonadIO,Alternative,MonadPlus)

first_  :: Functor f => (a -> f b) -> (a, c) -> f (b, c)
first_  f (x,y) = fmap (,y) (f x)

-- | Run a writer computation in the underlying monad.
runWriterT
    :: Monoid s
    => WriterT s m a -> m (a, s)
runWriterT =
    (coerce :: (StateT s m a -> m (a, s)) -> WriterT s m a -> m (a, s))
        (`runStateT` mempty)

{-# INLINE runWriterT #-}

{-# ANN module "HLint: ignore Use second" #-}

-- | This pattern gives the newtype wrapper around 'StateT' the same interface
-- as 'Control.Monad.Writer.Strict.WriterT'. Unfortunately, GHC currently warns
-- that a function is incomplete wherever this pattern is used. This issue
-- should be solved in a future version of GHC, when the
-- <https://ghc.haskell.org/trac/ghc/ticket/8779 COMPLETE> pragma is
-- implemented.
pattern WriterT :: (Functor m, Monoid s) =>
        m (a, s) -> WriterT s m a

pattern WriterT x <- (runWriterT -> x)
  where WriterT y
          = WriterT_ (StateT (\ s -> fmap  (\(x,p) -> (x, mappend s p)) y))

-- | A type synonym for the plain (non-transformer) version of 'WriterT'. This
-- can be used as if it were defined as:
--
-- > newtype Writer w a = Writer { runWriter :: (a, w) }
type Writer s = WriterT s Identity

-- | This pattern gives the newtype wrapper around 'StateT' the same interface
-- as as if it was defined like so:
--
-- > newtype Writer w a = Writer { runWriter :: (a, w) }
--
-- Unfortunately GHC warns that a function is incomplete wherever this pattern
-- is used. This issue should be solved in a future version of GHC, when the
-- <https://ghc.haskell.org/trac/ghc/ticket/8779 COMPLETE> pragma is
-- implemented.
--
-- >>> execWriter $ traverse (\x -> Writer (x, [x])) [1..5]
-- [1,2,3,4,5]
pattern Writer :: Monoid s => (a, s) -> Writer s a

pattern Writer x <- (runWriter -> x)
  where Writer (y, p)
          = WriterT_ (StateT (\s -> Identity (y, mappend s p)))

-- | Run a writer computation.
--
-- >>> runWriter $ traverse (\x -> Writer (show x, [x])) [1..5]
-- (["1","2","3","4","5"],[1,2,3,4,5])
runWriter
    :: Monoid s
    => Writer s a -> (a, s)
runWriter =
    (coerce
       :: (WriterT s Identity a -> Identity (a, s))
       -> (WriterT s Identity a -> (a, s))
    ) runWriterT

{-# INLINE runWriter #-}

instance (Monoid s, Monad m) =>
         MonadWriter s (WriterT s m) where
    writer (x,s) = WriterT (pure (x, s))
    {-# INLINE writer #-}
    listen (WriterT_ s) = WriterT_ ((,) <$> s <*> get)
    {-# INLINE listen #-}
    pass (WriterT_ s) = WriterT_ (passS s)
      where
        passS = (=<<) (uncurry (<$) . fmap (modify . coerce))
    {-# INLINE pass #-}

instance MonadState s m =>
         MonadState s (WriterT w m) where
    get = lift get
    put = lift . put
    state = lift . state

-- | Run a writer computation in the underlying monad, and return its result.
evalWriterT
    :: (Monad m, Monoid s)
    => WriterT s m a -> m a
evalWriterT =
    (coerce :: (StateT s m a -> m a) -> WriterT s m a -> m a)
        (`evalStateT` mempty)

{-# INLINE evalWriterT #-}

-- | Run a writer computation in the underlying monad, and collect its output.
execWriterT
    :: (Monad m, Monoid s)
    => WriterT s m a -> m s
execWriterT =
    (coerce :: (StateT s m a -> m s) -> WriterT s m a -> m s)
        (`execStateT` mempty)

{-# INLINE execWriterT #-}

-- | Run a writer computation, and return its result.
evalWriter
    :: Monoid s
    => Writer s a -> a
evalWriter =
    (coerce :: (State s a -> a) -> Writer s a -> a) (`evalState` mempty)

{-# INLINE evalWriter #-}

-- | Run a writer computation, and collect its output.
execWriter
    :: Monoid s
    => Writer s a -> s
execWriter =
    (coerce :: (State s a -> s) -> Writer s a -> s) (`execState` mempty)

{-# INLINE execWriter #-}

instance (Foldable m, Monoid w) =>
         Foldable (WriterT w m) where
    foldMap f =
        foldMap
            (\(x,_) ->
                  f x) .
        runWriterT

instance (Traversable m, Monoid w) =>
         Traversable (WriterT w m) where
    traverse f x = WriterT <$> (traverse . first_) f (runWriterT x)

instance (Eq1 m, Eq w, Monoid w) =>
         Eq1 (WriterT w m) where
    liftEq eq x y =
        liftEq
            (\(xx,xy) (yx,yy) ->
                  eq xx yx && xy == yy)
            (runWriterT x)
            (runWriterT y)

instance (Ord1 m, Ord w, Monoid w) =>
         Ord1 (WriterT w m) where
    liftCompare cmp x y =
        liftCompare
            (\(xx,xy) (yx,yy) ->
                  cmp xx yx <> compare xy yy)
            (runWriterT x)
            (runWriterT y)

instance (Read w, Read1 m, Monoid w, Functor m) =>
         Read1 (WriterT w m) where
    liftReadsPrec rp rl =
        readsData $ readsUnaryWith (liftReadsPrec rp' rl') "WriterT" WriterT
      where
        rp' = liftReadsPrec2 rp rl readsPrec readList
        rl' = liftReadList2 rp rl readsPrec readList

instance (Show w, Show1 m, Monoid w) =>
         Show1 (WriterT w m) where
    liftShowsPrec sp sl d m =
        showsUnaryWith (liftShowsPrec sp' sl') "WriterT" d (runWriterT m)
      where
        sp' = liftShowsPrec2 sp sl showsPrec showList
        sl' = liftShowList2 sp sl showsPrec showList

instance (Eq w, Eq1 m, Eq a, Monoid w) =>
         Eq (WriterT w m a) where
    (==) = eq1

instance (Ord w, Ord1 m, Ord a, Monoid w) =>
         Ord (WriterT w m a) where
    compare = compare1

instance (Read w, Read1 m, Read a, Monoid w, Functor m) =>
         Read (WriterT w m a) where
    readsPrec = readsPrec1

instance (Show w, Show1 m, Show a, Monoid w) =>
         Show (WriterT w m a) where
    showsPrec = showsPrec1
