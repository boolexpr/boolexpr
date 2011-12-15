{-# LANGUAGE GeneralizedNewtypeDeriving, TypeOperators #-}
module Data.Boolean where

import Control.Applicative
import Data.Foldable (Foldable, toList)
import Data.Monoid

infix /\
infix \/

-- | A boolean type class.
-- Minimal definition:
--   ('true' and 'false') or fromBool
class FromBool a where
  
  -- | The true boolean formula
  true  :: a
  true  = fromBool True

  -- | The false boolean formula
  false :: a
  false = fromBool False

  -- | Given a 'Bool' make a formula
  fromBool :: Bool -> a
  fromBool b = if b then true else false

class Conjunctive a where
  -- | Take the conjonction of two formulae
  ( /\ ) :: a -> a -> a

  -- | Take the conjonction of boolean formulae
  evalConj :: Conj a -> a

  all :: (Functor f, Foldable f) => (b -> a) -> f b -> a
  all f = evalConj . Conj . toList . fmap f

class Disjunctive a where
  -- | Take the disjunction of two formulae
  ( \/ ) :: a -> a -> a
  
  -- | Take the disjunction of boolean formulae
  evalDisj :: Disj a -> a

  any :: (x -> a) -> [x] -> a
  any f = evalDisj . Disj . fmap f

class Negative a where
  -- | Negate a formula
  neg :: a -> a

class Implicative a where
  -- | Forumlae implication
  imply :: a -> a -> a

-- | A boolean type class, which simply inherit from
-- 'FromBool', 'Conjunctive', 'Disjunctive', and 'Negative'.
class (FromBool a, Conjunctive a, Disjunctive a, Negative a, Implicative a) => Boolean a

-- | Default '/\' definition using De-Morgan law
andDefault :: (Negative a, Disjunctive a) => a -> a -> a
andDefault x y = neg (neg x \/ neg y)

-- | Default 'evalConj' definition using De-Morgan law
evalConjDefault :: (FromBool a, Conjunctive a) => Conj a -> a
evalConjDefault = foldr (/\) true . unConj

-- | Default '\/' definition using De-Morgan law
orDefault :: (Negative a, Conjunctive a) => a -> a -> a
orDefault x y = neg (neg x /\ neg y)

-- | Take the disjunction of boolean formulae
evalDisjDefault :: (FromBool a, Disjunctive a) => Disj a -> a
evalDisjDefault = foldr (\/) false . unDisj

-- | Forumlae implication
implyDefault :: (Disjunctive a, Negative a) => a -> a -> a
p `implyDefault` q = neg p \/ q

-- * 'Bool' instances

instance FromBool Bool where
  true     = True
  false    = False
  fromBool = id
instance Conjunctive Bool where
  (/\)     = (&&)
  evalConj = and . unConj
instance Disjunctive Bool where
  (\/)     = (||)
  evalDisj = or . unDisj
instance Negative Bool where
  neg      = not
instance Implicative Bool where
  p `imply` q = not p || q
instance Boolean Bool where

instance FromBool a => FromBool (Const a b) where
  true  = Const true
  false = Const false
instance Conjunctive a => Conjunctive (Const a b) where
  Const x /\ Const y = Const (x /\ y)
  evalConj           = Const . evalConj . Conj . fmap getConst . unConj
instance Disjunctive a => Disjunctive (Const a b) where
  Const x \/ Const y = Const (x \/ y)
  evalDisj           = Const . evalDisj . Disj . fmap getConst . unDisj
instance Negative a => Negative (Const a b) where
  neg = Const . neg . getConst
instance Implicative a => Implicative (Const a b) where
  Const p `imply` Const q = Const (p `imply` q)
instance Boolean a => Boolean (Const a b) where

--- | Disjunction of atoms ('a')
-- Note that 'Conj' is a 'Monad' and a 'Monoid', in particular
-- concatenation is done using 'mappend' (and 'mconcat') and
-- flattening a disjunction of disjunctions is done with 'join'.
newtype Disj a = Disj { unDisj :: [a] }
  deriving (Eq, Ord, Show, Functor, Monoid, Applicative, Monad)

instance Disjunctive (Disj a) where
  (\/)     = mappend
  evalDisj = mconcat . unDisj

--- | Conjunction of atoms ('a')
-- Note that 'Conj' is a 'Monad' and a 'Monoid', in particular
-- concatenation is done using 'mappend' (and 'mconcat') and
-- flattening a conjonction of conjonctions is done with 'join'.
newtype Conj a = Conj { unConj :: [a] }
  deriving (Eq, Ord, Show, Functor, Monoid, Applicative, Monad)

instance Conjunctive (Conj a) where
  (/\) = mappend
  evalConj = mconcat . unConj

--- | Disjunction of two atoms ('a' and 'b')
data a :\/: b = a :\/: b
  deriving (Eq, Ord, Show)

--- | Conjunction of two atoms ('a' and 'b')
data a :/\: b = a :/\: b
  deriving (Eq, Ord, Show)
