{-# LANGUAGE GeneralizedNewtypeDeriving #-}
--------------------------------------------------------------------
-- |
-- Module    : Data.BoolExpr
-- Copyright : (c) Nicolas Pouillard 2008,2009
-- License   : BSD3
--
-- Maintainer: Nicolas Pouillard <nicolas.pouillard@gmail.com>
-- Stability : provisional
-- Portability:
--
-- Boolean expressions and various representations.
--------------------------------------------------------------------

module Data.BoolExpr
  (-- * A boolean class
   Boolean(..)
   -- * Boolean trees
  ,BoolExpr(..)
  ,reduceBoolExpr
  ,evalBoolExpr
   -- * Signed constants
  ,Signed(..)
  ,negateSigned
  ,constants
  ,NegateConstant
  ,negateConstant
   -- * Conjunctive Normal Form
  ,CNF(..),Conj(..)
  ,boolTreeToCNF
  -- ,reduceCNF
   -- * Disjunctive Normal Form
  ,Disj(..),DNF(..)
  ,boolTreeToDNF
  -- ,reduceDNF
   -- * Other transformations
  ,dualize
  ,fromBoolExpr
  ,pushNotInwards
  )
  where

-- import Test.QuickCheck hiding (Positive)
-- import Control.Applicative
import Data.Monoid (Monoid(..))
import Data.Foldable (Foldable(..))
import Data.Traversable


-- | Signed values are either positive or negative.
data Signed a = Positive a | Negative a
  deriving (Eq, Ord, Show, Read)

instance Functor Signed where
  fmap f (Positive x) = Positive (f x)
  fmap f (Negative x) = Negative (f x)


infix /\
infix \/

-- | A boolean type class.
class Boolean f where
  ( /\ ) :: f a -> f a -> f a
  ( \/ ) :: f a -> f a -> f a
  bNot   :: f a -> f a
  bTrue  :: f a
  bFalse :: f a
  bConst :: a -> f a

-- | Syntax of boolean expressions parameterized over a
-- set of leaves, named constants.
data BoolExpr a = BAnd (BoolExpr a) (BoolExpr a)
                | BOr  (BoolExpr a) (BoolExpr a)
                | BNot (BoolExpr a)
                | BTrue
                | BFalse
                | BConst (Signed a)
  deriving (Eq, Ord, Show) {-! derive : Arbitrary !-}

instance Functor BoolExpr where
  fmap f (BAnd a b) = BAnd (fmap f a) (fmap f b)
  fmap f (BOr  a b) = BOr  (fmap f a) (fmap f b)
  fmap f (BNot t  ) = BNot (fmap f t)
  fmap _  BTrue     = BTrue
  fmap _  BFalse    = BFalse
  fmap f (BConst (Positive x)) = BConst (Positive (f x))
  fmap f (BConst (Negative x)) = BConst (Negative (f x))

instance Traversable BoolExpr where
  traverse f (BAnd a b) = BAnd <$> traverse f a <*> traverse f b
  traverse f (BOr  a b) = BOr  <$> traverse f a <*> traverse f b
  traverse f (BNot t  ) = BNot <$> traverse f t
  traverse _  BTrue     = pure BTrue
  traverse _  BFalse    = pure BFalse
  traverse f (BConst (Positive x)) = BConst <$> Positive <$> f x
  traverse f (BConst (Negative x)) = BConst <$> Negative <$> f x

instance Foldable BoolExpr where
  foldMap = foldMapDefault

instance Boolean BoolExpr where
  ( /\ ) = BAnd
  ( \/ ) = BOr
  bNot   = BNot
  bTrue  = BTrue
  bFalse = BFalse
  bConst = bSignedConst . Positive

-- | Turns a boolean tree into any boolean type.
fromBoolExpr :: Boolean f => BoolExpr a -> f (Signed a)
fromBoolExpr (BAnd l r) = fromBoolExpr l /\ fromBoolExpr r
fromBoolExpr (BOr  l r) = fromBoolExpr l \/ fromBoolExpr r
fromBoolExpr (BNot t  ) = bNot $ fromBoolExpr t
fromBoolExpr  BTrue     = bTrue
fromBoolExpr  BFalse    = bFalse
fromBoolExpr (BConst c) = bConst c

--- | Disjunction of atoms ('a')
newtype Disj a = Disj { unDisj :: [a] }
  deriving (Show, Functor, Monoid)

--- | Conjunction of atoms ('a')
newtype Conj a = Conj { unConj :: [a] }
  deriving (Show, Functor, Monoid)

--- | Conjunctive Normal Form
newtype CNF a = CNF { unCNF :: Conj (Disj (Signed a)) }
  deriving (Show, Monoid)

--- | Disjunctive Normal Form
newtype DNF a = DNF { unDNF :: Disj (Conj (Signed a)) }
  deriving (Show, Monoid)

instance Functor CNF where
  fmap f (CNF x) = CNF (fmap (fmap (fmap f)) x)

instance Boolean CNF where
  l /\ r = l `mappend` r
  l \/ r = CNF $ Conj [ x `mappend` y | x <- unConj $ unCNF l
                                      , y <- unConj $ unCNF r ]
  bNot     = error "bNot on CNF"
  bTrue    = CNF $ Conj[]
  bFalse   = CNF $ Conj[Disj[]]
  bConst x = CNF $ Conj[Disj[Positive x]]


instance Functor DNF where
  fmap f (DNF x) = DNF (fmap (fmap (fmap f)) x)

instance Boolean DNF where
  l /\ r = DNF $ Disj [ x `mappend` y | x <- unDisj $ unDNF l
                                      , y <- unDisj $ unDNF r ]
  l \/ r = l `mappend` r
  bNot     = error "bNot on CNF"
  bTrue    = DNF $ Disj[Conj[]]
  bFalse   = DNF $ Disj[]
  bConst x = DNF $ Disj[Conj[Positive x]]

-- | Reduce a boolean tree annotated by booleans to a single boolean.
reduceBoolExpr :: BoolExpr Bool -> Bool
reduceBoolExpr (BAnd a b) = reduceBoolExpr a && reduceBoolExpr b
reduceBoolExpr (BOr  a b) = reduceBoolExpr a || reduceBoolExpr b
reduceBoolExpr (BNot a)   = not $ reduceBoolExpr a
reduceBoolExpr  BTrue     = True
reduceBoolExpr  BFalse    = False
reduceBoolExpr (BConst (Positive c)) = c
reduceBoolExpr (BConst (Negative c)) = not c

-- Given a evaluation function of constants, returns an evaluation
-- function over boolean trees.
--
-- Note that since 'BoolExpr' is a functor, one can simply use
-- 'reduceBoolExpr':
--
-- @
-- evalBoolExpr f = reduceBoolExpr . fmap (f$)
-- @
evalBoolExpr :: (a -> Bool) -> (BoolExpr a -> Bool)
evalBoolExpr f = reduceBoolExpr . fmap (f$)

-- | Returns constants used in a given boolean tree, these
-- constants are returned signed depending one how many
-- negations stands over a given constant.
constants :: BoolExpr a -> [Signed (Signed a)]
constants = go True
  where go sign (BAnd a b) = go sign a ++ go sign b
        go sign (BOr  a b) = go sign a ++ go sign b
        go sign (BNot t)   = go (not sign) t
        go _     BTrue     = []
        go _     BFalse    = []
        go sign (BConst x) = [if sign then Positive x else Negative x]


dualize :: NegateConstant a -> BoolExpr a -> BoolExpr a
dualize neg (BAnd l r) = BOr  (dualize neg l) (dualize neg r)
dualize neg (BOr  l r) = BAnd (dualize neg l) (dualize neg r)
dualize _    BTrue     = BFalse
dualize _    BFalse    = BTrue
dualize neg (BConst c) = neg c
dualize _   (BNot _)   = error "dualize: impossible"


-- | Push the negations inwards as much as possible.
-- The resulting boolean tree no longer use negations.

pushNotInwards :: NegateConstant a -> BoolExpr a -> BoolExpr a
pushNotInwards neg (BAnd l r)   = BAnd (pushNotInwards neg l) (pushNotInwards neg r)
pushNotInwards neg (BOr  l r)   = BOr  (pushNotInwards neg l) (pushNotInwards neg r)
pushNotInwards neg (BNot t  )   = dualize neg $ pushNotInwards neg t
pushNotInwards _    BTrue       = BTrue
pushNotInwards _    BFalse      = BFalse
pushNotInwards _ b@(BConst _) = b


-- | Conversion functions
-- Convert a boolean tree to a conjunctive normal form.
boolTreeToCNF :: NegateConstant a -> BoolExpr a -> CNF (Signed a)
boolTreeToCNF neg = fromBoolExpr . pushNotInwards neg

-- | Convert a boolean tree to a disjunctive normal form.
boolTreeToDNF :: NegateConstant a -> BoolExpr a -> DNF (Signed a)
boolTreeToDNF neg = fromBoolExpr . pushNotInwards neg

-- | Reduce a boolean expression in conjunctive normal form to a single
-- boolean.
--reduceCNF :: CNF Bool -> Bool
--reduceCNF = all (or . unDisj) . unConj . unCNF

-- | Reduce a boolean expression in disjunctive normal form to a single
-- boolean.
--reduceDNF :: DNF Bool -> Bool
--reduceDNF = any (and . unConj) . unDisj . unDNF

type NegateConstant a = Signed a -> BoolExpr a


-- | SignedConst
bSignedConst :: Signed a -> BoolExpr a
bSignedConst x = BConst x
bSignedConst x = BNot (BConst x)

negateSigned :: Signed a -> Signed a
negateSigned (Positive x) = Negative x
negateSigned (Negative x) = Positive x

negateConstant :: Signed a -> BoolExpr a
negateConstant = bSignedConst . negateSigned


-- | Print
printer :: (a -> ShowS) -> BoolExpr a -> ShowS
printer = undefined

{-
prop_reduceBoolExpr_EQ_reduceCNF neg t = reduceBoolExpr t == reduceCNF (boolTreeToCNF neg t)

prop_reduceBoolExpr_EQ_reduceCNF_Bool = prop_reduceBoolExpr_EQ_reduceCNF (BConst . not)

prop_reduceBoolExpr_EQ_reduceDNF neg t = reduceBoolExpr t == reduceDNF (boolTreeToDNF neg t)

prop_reduceBoolExpr_EQ_reduceDNF_Bool = prop_reduceBoolExpr_EQ_reduceDNF (BConst . not)

{-* Generated by DrIFT : Look, but Don't Touch. *-}
instance (Arbitrary a) => Arbitrary (BoolExpr a) where
    arbitrary = do x <- choose (1::Int,6) -- :: Int inserted manually
                   case x of
                     1 -> do  v1 <- arbitrary
                              v2 <- arbitrary
                              return (BAnd v1 v2)
                     2 -> do  v1 <- arbitrary
                              v2 <- arbitrary
                              return (BOr v1 v2)
                     3 -> do  v1 <- arbitrary
                              return (BNot v1)
                     4 -> do  return (BTrue )
                     5 -> do  return (BFalse )
                     6 -> do  v1 <- arbitrary
                              return (BConst v1)
    --coarbitrary = error "coarbitrary not yet supported" -- quickcheck2
-}
