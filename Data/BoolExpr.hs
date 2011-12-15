{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}

-- TODO try a better algo to make a CNF (cf ben)


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
  ,fromBoolExpr
  ,evalBoolExpr
   -- * Signed constants
  ,Signed(..)
  ,constants
   -- * Conjunctive Normal Form
  ,CNF(..),Conj(..),(:/\:)(..)
  ,boolExprToCNF
  ,reduceCNF
   -- * Disjunctive Normal Form
  ,DNF(..),Disj(..),(:\/:)(..)
  ,boolExprToDNF
  ,reduceDNF
   -- * Other transformations
  ,dualize
  ,pushNotInwards
   -- * Search related experimental function
  ,naivePrepareSearch
  ,prepareSearch
  )
  where

import Prelude hiding (foldr)
-- import Test.QuickCheck hiding (Positive)
import Control.Applicative
import Control.Monad
-- import Control.Arrow ((&&&))
import Data.Monoid (Monoid(..))
import Data.Foldable (Foldable(..))
-- import Data.List (sortBy)
-- import Data.Function (on)

-- | Turns an atomic constant into a boolean formula.
-- Often this function is redundant with 'return' and 'pure',
-- and if there were a standard Pointed type class it would also.
bConst :: Applicative f => a -> f a
bConst = pure

{-
class QuantifiedFormula f where
  qForall :: (f a -> f a) -> f a
  qExists :: (f a -> f a) -> f a

data QFormula a where
  QBoolExpr :: BoolExpr a -> QFormula a
  QForall   :: QFormula (Maybe a) -> QFormula a

instance QuantifiedFormula QFormula where
  qForall f = f 
-}

-- NOTE: find another name
-- class (Functor f, Applicative f, Monad f, PointedBoolean f) => BooleanF f where


class EvalAlgebra f where
  evalAlg :: f a -> a

-- | Negation Normal Form
data NNF a = NNFAnd (NNF a) (NNF a)
           | NNFOr  (NNF a) (NNF a)
           | NNFLit (Signed a)
  deriving (Eq, Ord, Show)
