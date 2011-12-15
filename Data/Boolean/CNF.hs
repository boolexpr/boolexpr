{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
module Data.Boolean.CNF where

import Prelude
import qualified Prelude as P
import Data.Boolean
import Data.Boolean.Signed
import Data.Boolean.BoolExpr
import Data.Monoid
import Control.Monad
import Control.Applicative

--- | Conjunctive Normal Form
newtype CNF a = CNF { unCNF :: Conj (Disj (Signed a)) }
  deriving (Show, Monoid)

clauses :: CNF a -> [Disj (Signed a)]
clauses = unConj . unCNF

-- | Convert a boolean tree to a conjunctive normal form.
boolExprToCNF :: BoolExpr a -> CNF a
boolExprToCNF = evalBoolExpr . fmap (evalSigned . fmap pure) . pushNotInwards

instance Functor CNF where
  fmap f = CNF . fmap (fmap (fmap f)) . unCNF

instance Monad CNF where
  return  = CNF . Conj . pure . Disj . pure . Positive
  x >>= f = joinCNF (fmap f x)
    where joinCNF = boolExprToCNF . join . fromCNF . fmap fromCNF

instance Applicative CNF where
  pure  = return
  (<*>) = ap

instance FromBool (CNF a) where
  true  = CNF $ Conj []
  false = CNF $ Conj [Disj []]

instance Negative (CNF a) where
  neg = fromBoolExpr . neg . fromCNF

instance Conjunctive (CNF a) where
  (/\) = mappend
  evalConj = mconcat . unConj

instance Disjunctive (CNF a) where
  l \/ r = CNF $ Conj [ x `mappend` y | x <- clauses l
                                      , y <- clauses r ]
  evalDisj = CNF . Conj . fmap (mconcat . clauses) . unDisj
  
instance Implicative (CNF a) where
  imply = implyDefault

instance Boolean (CNF a)

class FromCNF f where
  fromCNF :: CNF a -> f a

instance FromCNF CNF where
  fromCNF = id

instance FromCNF BoolExpr where
  fromCNF = fromCNFDefault

class EvalCNF a where
  evalCNF :: CNF a -> a

instance (Negative a, Disjunctive a, Conjunctive a) => EvalCNF (BoolExpr a) where
  evalCNF = evalCNFDefault

fromCNFDefault :: (Applicative f, Boolean (f a)) => CNF a -> f a
fromCNFDefault = evalCNFDefault . fmap pure

evalCNFDefault :: (Negative a, Disjunctive a, Conjunctive a) => CNF a -> a
evalCNFDefault = evalConj . fmap (evalDisj . fmap evalSigned) . unCNF

instance EvalCNF Bool where
  evalCNF = P.all (P.any evalSigned . unDisj) . clauses
