-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Simplification logic for boolean expressions.
module Data.BoolExpr.Simplify (
  cannotBeTrue,
  subst,
  substMap,
) where

import Data.BoolExpr
import qualified Data.List as L
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S

-- | Used only by "hasContradiction".
-- Note that the Booleans returned in this tuple are not actually used
-- as conditions, and therefore their semantic convention (e.g. associating
-- True = Positive and False = Negative) is irrelevant.
-- Rather, they are collected into sets
-- to determine whether both True and False exist for a key.
extractConstFromSigned :: Signed a -> (a, Bool)
extractConstFromSigned v = case v of
  Negative x -> (x, False)
  Positive x -> (x, True)

-- | Places the signednesses for each constant into bins.
-- Any bin that has both kinds of signedness represents a contradiction.
hasContradiction :: Ord a => Conj (Signed a) -> Bool
hasContradiction (Conj items) =
  not $
    M.null $
      M.filter ((> 1) . S.size) $
        M.fromListWith (<>) $
          fmap (fmap S.singleton . extractConstFromSigned) items

-- | Eliminates any of the disjunctions that contain
-- contradictions, since they are False and irrelevant to an OR.
simplifyDNF :: Ord a => DNF a -> DNF a
simplifyDNF (DNF (Disj disjunctions)) =
  DNF $ Disj $ L.filter (not . hasContradiction) disjunctions

-- | A DNF expression with no terms is considered False.
isVacuouslyFalse :: Ord a => DNF a -> Bool
isVacuouslyFalse (DNF (Disj disjunctions)) = L.null disjunctions

-- | Converts to DNF and checks whether all of the terms are contradictions.
cannotBeTrue :: Ord a => BoolExpr a -> Bool
cannotBeTrue = isVacuouslyFalse . simplifyDNF . boolTreeToDNF

-- | Apply a function to promote constants to arbitrary
-- expressions. A generalization of "substMap".
subst :: (Signed a -> BoolExpr a) -> BoolExpr a -> BoolExpr a
subst f (BAnd a b) = BAnd (subst f a) (subst f b)
subst f (BOr a b) = BOr (subst f a) (subst f b)
subst f (BNot t) = BNot (subst f t)
subst _ BTrue = BTrue
subst _ BFalse = BFalse
subst f (BConst x) = f x

-- | Apply Boolean value substitutions to named constants in
-- the expression using the supplied map.
-- The substituted constants are promoted to Boolean literals,
-- accounting for signedness of the constant.
substMap :: Ord a => Map a Bool -> BoolExpr a -> BoolExpr a
substMap m = subst f
  where
    f x = case M.lookup varname m of
      Nothing -> BConst x
      Just val ->
        if txform val
          then BTrue
          else BFalse
      where
        (varname, isPositive) = extractConstFromSigned x
        txform = if isPositive then id else not

