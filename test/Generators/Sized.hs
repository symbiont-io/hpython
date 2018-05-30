{-# language GeneralizedNewtypeDeriving, DeriveFunctor #-}
module Generators.Sized where

import Control.Applicative (Alternative, (<|>))
import Control.Monad (when)
import Data.List.NonEmpty (NonEmpty(..))

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Hedgehog.Internal.Shrink as Shrink

import GHC.Stack (HasCallStack)

growable :: (HasCallStack, MonadGen m, Alternative m) => Size -> Size -> m a -> m (Size, a)
growable sz mx ma =
  (,) sz <$> Gen.resize sz ma <|>
  do
     when (sz == mx) $ error "test" -- Gen.discard
     sz' <- Gen.integral_ $ Range.constant sz mx
     growable sz' mx ma

sized2M
  :: (MonadGen m, Alternative m)
  => (a -> b -> m c)
  -> m a
  -> m b
  -> m c
sized2M f ma mb =
  Gen.sized $ \n -> do
    when (n < 2) Gen.discard
    aSize <- Gen.integral (Range.constant 1 (n-1))
    (a_actual, a) <- growable aSize (n-1) ma
    (_, b) <- growable (n - a_actual) a_actual mb
    f a b

sized2
  :: (MonadGen m, Alternative m)
  => (a -> b -> c)
  -> m a
  -> m b
  -> m c
sized2 f = sized2M (\a b -> pure $ f a b)

sized3M
  :: (MonadGen m, Alternative m)
  => (a -> b -> c -> m d)
  -> m a
  -> m b
  -> m c
  -> m d
sized3M f ma mb mc =
  Gen.sized $ \n -> do
    when (n < 3) Gen.discard
    abSize <- Gen.integral (Range.constant 1 (n-2))
    cd <- Gen.resize abSize $ sized2 f ma mb
    c <- Gen.resize (n - abSize) mc
    cd c

sized3
  :: (MonadGen m, Alternative m)
  => (a -> b -> c -> d)
  -> m a
  -> m b
  -> m c
  -> m d
sized3 f = sized3M (\a b c -> pure $ f a b c)

sized4M
  :: (MonadGen m, Alternative m)
  => (a -> b -> c -> d -> m e)
  -> m a
  -> m b
  -> m c
  -> m d
  -> m e
sized4M f ma mb mc md =
  Gen.sized $ \n -> do
    when (n < 4) Gen.discard
    abcSize <- Gen.integral (Range.constant 1 (n-3))
    de <- Gen.resize abcSize $ sized3 f ma mb mc
    d <- Gen.resize (n - abcSize) md
    de d

sized4
  :: (MonadGen m, Alternative m)
  => (a -> b -> c -> d -> e)
  -> m a
  -> m b
  -> m c
  -> m d
  -> m e
sized4 f = sized4M (\a b c d -> pure $ f a b c d)

sizedList :: (MonadGen m, Alternative m) => m a -> m [a]
sizedList ma =
  Gen.shrink Shrink.list $
  sized2 (:)
    ma
    (Gen.sized $ \n -> if n == 0 then pure [] else sizedList ma)

sizedNonEmpty :: (MonadGen m, Alternative m) => m a -> m (NonEmpty a)
sizedNonEmpty ma =
  Gen.shrink (\(x :| xs) -> (x :|) <$> Shrink.list xs) $
  Gen.sized $ \n -> do
    when (n < 1) Gen.discard
    sized2 (:|)
      ma
      (sizedList ma)

sizedMaybe :: MonadGen m => m a -> m (Maybe a)
sizedMaybe ma =
  Gen.sized $ \n ->
    if n == 0 then pure Nothing else Gen.maybe (Gen.resize (n-1) ma)

thresholds :: (HasCallStack, MonadGen m) => [(Maybe Size, m a)] -> m a
thresholds [] = error "empty list in thresholds"
thresholds mas =
  Gen.sized $ \n -> do
    let mas' = filter (maybe True (n >=) . fst) mas
    case fmap (\(_, ma) -> Gen.scale (max 0 . subtract 1) ma) mas' of
      [] -> Gen.discard
      mas'' -> Gen.choice mas''
