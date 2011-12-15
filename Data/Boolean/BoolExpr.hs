module Data.Boolean.BoolExpr where

import Data.Boolean
import Data.Boolean.Signed
import Data.Set (Set, union, intersection, difference)
import Data.Foldable (Foldable(..))
import Data.Traversable
import Data.Monoid
import Control.Applicative
import Control.Monad
-- import Control.Monad.Identity
import Test.QuickCheck hiding (Positive)

-- | Syntax of boolean expressions parameterized over a
-- set of leaves, named constants.
data BoolExpr a = And (BoolExpr a) (BoolExpr a)
                | Or  (BoolExpr a) (BoolExpr a)
                | Not (BoolExpr a)
                | T
                | F
                | Lit a
  deriving (Eq, Ord, Show) {-! derive : Arbitrary !-}

instance FromBool    (BoolExpr a) where
  true  = T
  false = F
instance Negative    (BoolExpr a) where
  neg  = Not
instance Conjunctive (BoolExpr a) where
  (/\) = And
  evalConj = evalConjDefault
instance Disjunctive (BoolExpr a) where
  (\/) = Or
  evalDisj = evalDisjDefault
instance Implicative (BoolExpr a) where
  imply = implyDefault
instance Boolean     (BoolExpr a)

instance Functor BoolExpr where
  fmap f (a `And` b) = fmap f a /\ fmap f b
  fmap f (a `Or`  b) = fmap f a \/ fmap f b
  fmap f (Not t)     = neg (fmap f t)
  fmap _ T           = true
  fmap _ F           = false
  fmap f (Lit x)     = pure (f x)

instance Traversable BoolExpr where
  traverse f (a `And` b) = (/\) <$> traverse f a <*> traverse f b
  traverse f (a `Or`  b) = (\/) <$> traverse f a <*> traverse f b
  traverse f (Not t)     = neg  <$> traverse f t
  traverse _ T           = pure true
  traverse _ F           = pure false
  traverse f (Lit x)     = pure <$> f x

instance Foldable BoolExpr where
  foldMap = foldMapDefault

instance Monad BoolExpr where
  return = Lit
  l `And` r >>= f = (l >>= f) /\ (r >>= f)
  l `Or`  r >>= f = (l >>= f) \/ (r >>= f)
  Not t     >>= f = neg . f =<< t
  T         >>= _ = true
  F         >>= _ = false
  Lit c     >>= f = f c

instance Applicative BoolExpr where
  pure = Lit
  f <*> l `And` r = (f <*> l) /\ (f <*> r)
  f <*> l `Or`  r = (f <*> l) \/ (f <*> r)
  f <*> Not t     = neg (f <*> t)
  _ <*> T         = true
  _ <*> F         = false
  f <*> Lit c     = ($c) <$> f

-- | Turns a boolean tree into any boolean type.
evalBoolExpr :: (Boolean a) => BoolExpr a -> a
evalBoolExpr (l `And` r) = evalBoolExpr l /\ evalBoolExpr r
evalBoolExpr (l `Or`  r) = evalBoolExpr l \/ evalBoolExpr r
evalBoolExpr (Not t)     = neg $ evalBoolExpr t
evalBoolExpr T           = true
evalBoolExpr F           = false
evalBoolExpr (Lit c)     = c

-- | Turns a boolean tree into any boolean type.
fromBoolExpr :: (Applicative f, Boolean (f a)) => BoolExpr a -> f a
fromBoolExpr = evalBoolExpr . fmap pure

-- | Returns constants used in a given boolean tree, these
-- constants are returned signed depending one how many
-- negations stands over a given constant.
constants :: BoolExpr a -> [Signed a]
constants = go True
  where go sign (a `And` b) = go sign a ++ go sign b
        go sign (a `Or`  b) = go sign a ++ go sign b
        go sign (Not t)     = go (not sign) t
        go _    T           = []
        go _    F           = []
        go sign (Lit x)     = [if sign then Positive x else Negative x]

prepareSearch, naivePrepareSearch :: Ord b => Set b -> (a -> Set b) -> (BoolExpr a -> Set b)
naivePrepareSearch universe f = go . pushNotInwards
  where go (a `And` b) = go a `intersection` go b
        go (a `Or`  b) = go a `union` go b
        go T           = universe
        go F           = mempty
        go (Lit x)     = s x
        go (Not _)     = error "naivePreapreSearch: Not"
        s  (Positive x) = f x
        s  (Negative x) = universe `difference` f x

prepareSearch = naivePrepareSearch

{-
orderedCNF :: (a -> Int)        -- ^ size approximation of the search result
           -> (a -> BoolExpr a) -- ^ atom negation
           -> BoolExpr a
           -> Maybe (Disj a :/\: CNF a)
orderedCNF approx neg t = cut sortedCnf
  where (CNF cnf) = boolExprToCNF neg t
        sortedCnf = map fst $ sortBy (on snd) $ fmap (id &&& (sum . fmap approx)) cnf
        cut []     = Nothing
        cut (x:xs) = Just $ x :/\: (CNF $ Conj $ reverse xs)
-}


-- Given a evaluation function of constants, returns an evaluation
-- function over boolean trees.
--
-- Note that since 'BoolExpr' is a functor, one can simply use
-- 'evalBoolExpr':
--
-- @
-- evalBoolPred f = evalBoolExpr . fmap (f$)
-- @
evalBoolExprPred :: (a -> Bool) -> (BoolExpr a -> Bool)
evalBoolExprPred f = evalBoolExpr . fmap (f$)

dualize :: BoolExpr (Signed a) -> BoolExpr (Signed a)
dualize (l `And` r) = dualize l \/ dualize r
dualize (l `Or`  r) = dualize l /\ dualize r
dualize T      = false
dualize F     = true
dualize (Lit c) = pure (neg c)
dualize (Not _)     = error "dualize: Not impossible"

-- | Push the negations inwards as much as possible.
-- The resulting boolean tree no longer use negations.
pushNotInwards :: BoolExpr a -> BoolExpr (Signed a)
pushNotInwards (l `And` r)   = pushNotInwards l /\ pushNotInwards r
pushNotInwards (l `Or`  r)   = pushNotInwards l \/ pushNotInwards r
pushNotInwards (Not (Not t)) = pushNotInwards t
pushNotInwards (Not t)       = dualize $ pushNotInwards t
pushNotInwards T         = true
pushNotInwards F        = false
pushNotInwards (Lit c)    = pure (Positive c)


arbitraryBoolean :: (Arbitrary a, Monad f, Boolean (f a)) => Gen (f a)
arbitraryBoolean
  = do x <- choose (1::Int,6)
       case x of
         1 -> liftM2 (/\) arbitraryBoolean arbitraryBoolean
         2 -> liftM2 (\/) arbitraryBoolean arbitraryBoolean
         3 -> neg <$> arbitraryBoolean
         4 -> return true
         5 -> return false
         _ -> return <$> arbitrary

{-
prop_reduceBoolExpr_EQ_reduceCNF neg t = reduceBoolExpr t == reduceCNF (boolExprToCNF neg t)

prop_reduceBoolExpr_EQ_reduceCNF_Bool = prop_reduceBoolExpr_EQ_reduceCNF (Lit . not)

prop_reduceBoolExpr_EQ_reduceDNF neg t = reduceBoolExpr t == reduceDNF (boolExprToDNF neg t)

prop_reduceBoolExpr_EQ_reduceDNF_Bool = prop_reduceBoolExpr_EQ_reduceDNF (Lit . not)

{-* Generated by DrIFT : Look, but Don't Touch. *-}
instance (Arbitrary a) => Arbitrary (BoolExpr a) where
    arbitrary = arbitraryBoolean
    --coarbitrary = error "coarbitrary not yet supported" -- quickcheck2
-}
