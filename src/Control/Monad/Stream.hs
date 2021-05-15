-- |
-- Module      : Control.Monad.Stream
-- Copyright   : Oleg Kiselyov, Sebastian Fischer, David A Roberts
-- License     : BSD3
-- 
-- Maintainer  : David A Roberts <d@vidr.cc>
-- Stability   : experimental
-- Portability : portable
-- 
-- This Haskell library provides an implementation of the MonadPlus
-- type class that enumerates results of a non-deterministic
-- computation by interleaving subcomputations in a way that has
-- usually much better memory performance than other strategies with
-- the same termination properties.
-- 
-- By using supensions in strategic positions, the user can ensure
-- that the search does not diverge if there are remaining
-- non-deterministic results.
-- 
-- More information is available on the author's website:
-- <http://okmij.org/ftp/Computation/monads.html#fair-bt-stream>
-- 
-- Warning: @Stream@ is only a monad when the results of @observeAll@
-- are interpreted as a multiset, i.e., a valid transformation
-- according to the monad laws may change the order of the results.
-- 
{-# LANGUAGE CPP, FlexibleInstances, LambdaCase,
  MultiParamTypeClasses, UndecidableInstances #-}

module Control.Monad.Stream
  ( MonadLogic(..)
  , StreamT
  , Stream
  , suspended
  , runStream
  , toList
  , observe
  , observeT
  , observeAll
  , observeAllT
  , observeMany
  , observeManyT
  ) where

import Control.Applicative (Alternative(..), (<**>))
import Control.Monad (MonadPlus(..), liftM)
import qualified Control.Monad.Fail as Fail
import Control.Monad.Identity (Identity(..))
import Control.Monad.Logic.Class (MonadLogic(..))
import Control.Monad.Reader.Class (MonadReader(..))
import Control.Monad.State.Class (MonadState(..))
import Control.Monad.Trans (MonadIO(..), MonadTrans(..))
import qualified Data.Foldable as F
import Data.Foldable (toList)
#if !MIN_VERSION_base(4,8,0)
import Data.Monoid (Monoid(..))
#endif
#if MIN_VERSION_base(4,9,0)
import Data.Semigroup (Semigroup(..))
#endif

data StreamF s a
  = Nil
  | Single a
  | Cons a s
  | Susp s

-- |
-- Results of non-deterministic computations of type @StreamT m a@ can be
-- enumerated efficiently.
-- 
newtype StreamT m a =
  StreamT
    { unStreamT :: m (StreamF (StreamT m a) a)
    }

type Stream = StreamT Identity

-- |
-- Suspensions can be used to ensure fairness.
-- 
suspended :: Monad m => StreamT m a -> StreamT m a
suspended = StreamT . return . Susp

cons :: Monad m => a -> StreamT m a -> StreamT m a
cons a = StreamT . return . Cons a

bind ::
     Monad m
  => StreamT m a
  -> (StreamF (StreamT m a) a -> StreamT m b)
  -> StreamT m b
bind m f = StreamT $ unStreamT m >>= unStreamT . f

-- |
-- The function @runStream@ enumerates the results of a
-- non-deterministic computation.
-- 
runStream :: Stream a -> [a]
runStream = observeAll

{-# DEPRECATED
runStream "use observeAll"
 #-}

instance Monad m => Monad (StreamT m) where
  return = pure
  m >>= f =
    m `bind` \case
      Nil -> empty
      Single x -> f x
      Cons x xs -> f x <|> suspended (xs >>= f)
      Susp xs -> suspended (xs >>= f)
#if !MIN_VERSION_base(4,13,0)
  fail = Fail.fail
#endif
instance Monad m => Fail.MonadFail (StreamT m) where
  fail _ = empty

instance Monad m => Alternative (StreamT m) where
  empty = StreamT $ return Nil
  m <|> ys =
    m `bind` \case
      Nil -> suspended ys -- suspending
      Single x -> cons x ys
      Cons x xs -> cons x (ys <|> xs) -- interleaving
      Susp xs ->
        ys `bind` \case
          Nil -> suspended xs
          Single y -> cons y xs
          Cons y ys' -> cons y (xs <|> ys')
          Susp ys' -> suspended (xs <|> ys')

instance Monad m => MonadPlus (StreamT m) where
  mzero = empty
  mplus = (<|>)
#if MIN_VERSION_base(4,9,0)
instance Monad m => Semigroup (StreamT m a) where
  (<>) = mplus
  sconcat = foldr1 mplus
#endif
instance Monad m => Monoid (StreamT m a) where
  mempty = empty
  mappend = (<|>)
  mconcat = F.asum

instance Monad m => Functor (StreamT m) where
  fmap f m =
    m `bind` \case
      Nil -> empty
      Single x -> return (f x)
      Cons x xs -> cons (f x) (fmap f xs)
      Susp xs -> suspended (fmap f xs)

instance Monad m => Applicative (StreamT m) where
  pure = StreamT . return . Single
  m <*> xs =
    m `bind` \case
      Nil -> empty
      Single f -> fmap f xs
      Cons f fs -> fmap f xs <|> (xs <**> fs)
      Susp fs -> suspended (xs <**> fs)

instance Monad m => MonadLogic (StreamT m) where
  (>>-) = (>>=)
  interleave = mplus
  msplit m =
    m `bind` \case
      Nil -> return Nothing
      Single x -> return $ Just (x, empty)
      Cons x xs -> return $ Just (x, suspended xs)
      Susp xs -> suspended $ msplit xs

instance MonadTrans StreamT where
  lift = StreamT . liftM Single

instance MonadIO m => MonadIO (StreamT m) where
  liftIO = lift . liftIO

instance MonadReader r m => MonadReader r (StreamT m) where
  ask = lift ask
  local f = StreamT . local f . unStreamT

instance MonadState s m => MonadState s (StreamT m) where
  get = lift get
  put = lift . put

instance (Monad m, Foldable m) => Foldable (StreamT m) where
  foldMap f = foldMap g . unStreamT
    where
      g Nil = mempty
      g (Single x) = f x
      g (Cons x xs) = f x <> foldMap f xs
      g (Susp xs) = foldMap f xs

instance (Monad m, Traversable m) => Traversable (StreamT m) where
  traverse f = fmap StreamT . traverse g . unStreamT
    where
      g Nil = pure Nil
      g (Single x) = Single <$> f x
      g (Cons x xs) = Cons <$> f x <*> traverse f xs
      g (Susp xs) = Susp <$> traverse f xs

observeAllT :: Monad m => StreamT m a -> m [a]
observeAllT m =
  unStreamT m >>= \case
    Nil -> return []
    Single a -> return [a]
    Cons a r -> do
      t <- observeAllT r
      return (a : t)
    Susp r -> observeAllT r

observeAll :: Stream a -> [a]
observeAll = runIdentity . observeAllT

observeManyT :: Monad m => Int -> StreamT m a -> m [a]
observeManyT 0 _ = return []
observeManyT n m =
  unStreamT m >>= \case
    Nil -> return []
    Single a -> return [a]
    Cons a r -> do
      t <- observeManyT (n - 1) r
      return (a : t)
    Susp r -> observeManyT n r

observeMany :: Int -> Stream a -> [a]
observeMany n = runIdentity . observeManyT n

#if !MIN_VERSION_base(4,13,0)
observeT :: Monad m => StreamT m a -> m a
#else
observeT :: MonadFail m => StreamT m a -> m a
#endif
observeT m =
  unStreamT m >>= \case
    Nil -> fail "No answer."
    Single a -> return a
    Cons a _ -> return a
    Susp r -> observeT r

observe :: Stream a -> a
observe m =
  case runIdentity (unStreamT m) of
    Nil -> error "No answer."
    Single a -> a
    Cons a _ -> a
    Susp r -> observe r
