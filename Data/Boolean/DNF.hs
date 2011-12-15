{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Boolean.DNF where

import Data.Boolean hiding (any, all)
import Data.Boolean.Signed
import Data.Boolean.BoolExpr
import Data.Monoid
import Control.Applicative
import Control.Monad

--- | Disjunctive Normal Form
newtype DNF a = DNF { unDNF :: Disj (Conj (Signed a)) }
  deriving (Show, Monoid)

dclauses :: DNF a -> [Conj (Signed a)]
dclauses = unDisj . unDNF

instance Functor DNF where
  fmap f = DNF . fmap (fmap (fmap f)) . unDNF

instance Monad DNF where
  return  = DNF . Disj . pure . Conj . pure . Positive
  x >>= f = joinDNF (fmap f x)
    where joinDNF = boolExprToDNF . join . fromDNF . fmap fromDNF

instance Applicative DNF where
  pure  = return
  (<*>) = ap

instance FromBool (DNF a) where
  true  = DNF $ Disj [Conj []]
  false = DNF $ Disj []

instance Conjunctive (DNF a) where
  l /\ r = DNF $ Disj [ x `mappend` y | x <- dclauses l
                                      , y <- dclauses r ]
  evalConj = DNF . Disj . fmap (mconcat . dclauses) . unConj

instance Disjunctive (DNF a) where
  (\/) = mappend
  evalDisj = mconcat . unDisj

instance Negative (DNF a) where
  neg = fromBoolExpr . neg . fromDNF

instance Implicative (DNF a) where
  imply = implyDefault

instance Boolean (DNF a) where

-- | Convert a boolean tree to a disjunctive normal form.
boolExprToDNF :: BoolExpr a -> DNF a
boolExprToDNF = evalBoolExpr . fmap (evalSigned . fmap pure) . pushNotInwards

class FromDNF f where
  -- | Turns a 'DNF' into a boolean formula
  fromDNF :: DNF a -> f a

instance FromDNF DNF where
  fromDNF = id

instance FromDNF BoolExpr where
  fromDNF = fromDNFDefault

class EvalDNF a where
  -- | Turns a 'DNF' into a boolean formula
  evalDNF :: DNF a -> a

instance EvalDNF Bool where
  evalDNF = any (all evalSigned . unConj) . dclauses

evalDNFDefault :: Boolean a => DNF a -> a
evalDNFDefault = evalDisj . fmap (evalConj . fmap evalSigned) . unDNF

fromDNFDefault :: (Applicative f, Boolean (f a)) => DNF a -> f a
fromDNFDefault = evalDNFDefault . fmap pure
