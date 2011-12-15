module Data.Boolean.Signed where

import Data.Boolean
import Data.Foldable (Foldable(foldMap))
import Data.Traversable
import Control.Applicative

-- | Signed values are either positive of negative.
data Signed a = Positive a
              | Negative a
  deriving (Eq, Ord, Show, Read)

instance Functor Signed where
  fmap f (Positive x) = Positive (f x)
  fmap f (Negative x) = Negative (f x)

instance Negative (Signed a) where
  neg (Positive x) = Negative x
  neg (Negative x) = Positive x

instance Foldable Signed where
  foldMap = foldMapDefault

instance Traversable Signed where
  traverse f (Positive x) = Positive <$> f x
  traverse f (Negative x) = Negative <$> f x

instance Applicative Signed where
  pure                      = Positive
  Positive f <*> Positive x = Positive (f x)
  Positive f <*> Negative x = Negative (f x)
  Negative f <*> Negative x = Positive (f x)
  Negative f <*> Positive x = Negative (f x)

instance Monad Signed where
  return           = Positive
  Positive x >>= f = f x
  Negative x >>= f = neg (f x)

evalSigned :: Negative a => Signed a -> a
evalSigned (Positive x) = x
evalSigned (Negative x) = neg x
