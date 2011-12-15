{-# LANGUAGE GADTs, EmptyDataDecls, FlexibleContexts, TypeFamilies #-}
module Data.Boolean.BoolExpr.NFAC where

import Prelude hiding (mapM)
import Control.Applicative
import Data.Boolean
import Data.Boolean.Signed
import Data.Foldable (Foldable(foldMap))
import Data.Traversable

data AND
data OR

type family   NEG a :: *
type instance NEG AND = OR
type instance NEG OR  = AND

data Expr f a where
  T    :: Expr f a
  F    :: Expr f a
  And  :: f (ExprT OR f a) -> Expr f a
  Or   :: f (ExprT AND f a) -> Expr f a
  Lit  :: Signed a -> Expr f a

data ExprT t f a where
  AndT :: f (ExprT OR f a)  -> ExprT AND f a
  OrT  :: f (ExprT AND f a) -> ExprT OR f a
  LitT :: Signed a -> ExprT t f a

instance Functor f => Functor (ExprT t f) where
  fmap f (AndT xs) = AndT ((fmap . fmap) f xs)
  fmap f (OrT  xs) = OrT  ((fmap . fmap) f xs)
  fmap f (LitT x)  = LitT (fmap f x)

instance Functor f => Functor (Expr f) where
  fmap f (And xs) = And ((fmap . fmap) f xs)
  fmap f (Or  xs) = Or  ((fmap . fmap) f xs)
  fmap f (Lit x)  = Lit (fmap f x)
  fmap _ T        = T
  fmap _ F        = F

instance Traversable f => Foldable (ExprT t f) where
  foldMap = foldMapDefault

instance Traversable f => Foldable (Expr f) where
  foldMap = foldMapDefault

instance Traversable f => Traversable (ExprT t f) where
  traverse f (AndT xs) = AndT <$> traverse (traverse f) xs
  traverse f (OrT  xs) = OrT  <$> traverse (traverse f) xs
  traverse f (LitT t)  = LitT <$> traverse f t

instance Traversable f => Traversable (Expr f) where
  traverse f (And xs) = And <$> traverse (traverse f) xs
  traverse f (Or  xs) = Or  <$> traverse (traverse f) xs
  traverse f (Lit t)  = Lit <$> traverse f t
  traverse _ T        = pure true
  traverse _ F        = pure false

-- instance Monad (ExprT t f) where
--   return        = LitT . Positive
--   (>>=) = bind
--     where

{-
joinT :: Functor f => ExprT t f (Expr f a) -> ExprT t f a
joinT (AndT xs) = AndT (fmap joinT xs)
joinT (OrT  xs) = OrT  (fmap joinT xs)
joinT (LitT l)  = undefined
-}

toAND :: Alternative f => Expr f a -> ExprT AND f a
toAND (And xs) = AndT xs
toAND (Or xs)  = AndT (pure (OrT xs))
toAND (Lit l)  = AndT (pure (LitT l))
toAND T        = AndT empty
toAND F        = AndT (pure (OrT empty))

joinTAnd :: Alternative f => ExprT AND f (Expr f a) -> ExprT AND f a
joinTAnd (AndT xs) = AndT (fmap joinTOr  xs)
joinTAnd (LitT (Positive l))  = toAND l
joinTAnd (LitT (Negative _l))  = undefined

joinTOr :: Alternative f => ExprT OR f (Expr f a) -> ExprT OR f a
joinTOr (OrT  xs) = OrT  (fmap joinTAnd xs)
joinTOr (LitT _l)  = undefined -- LitT l

instance Alternative f => Monad (Expr f) where
  return       = Lit . Positive
  And xs >>= f = And (fmap (joinTOr . fmap f) xs)
  Or  xs >>= f = Or  (fmap (joinTAnd . fmap f) xs)
  T      >>= _ = true
  F      >>= _ = false
  Lit l  >>= f = evalSigned (f <$> l)


{-
instance Applicative (Expr f) where
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
-}

instance FromBool (Expr f a) where
  true  = T
  false = F

fromList :: Alternative f => [a] -> f a
fromList = foldr ((<|>) . pure) empty

instance Alternative f => Conjunctive (Expr f a) where
  T     /\ x     = x
  x     /\ T     = x
  F     /\ _     = F
  _     /\ F     = F
  And x /\ And y = And (x <|> y)
  And x /\ Or y  = And (x <|> pure (OrT y))  -- maybe we should swap <> args
  And x /\ Lit y = And (x <|> pure (LitT y)) -- maybe we should swap <> args
  Or  x /\ And y = And (pure (OrT  x) <|> y)
  Lit x /\ And y = And (pure (LitT x) <|> y)
  Or  x /\ Lit y = And (pure (OrT  x) <|> pure (LitT y))
  Lit x /\ Or  y = And (pure (LitT x) <|> pure (OrT y))
  Or  x /\ Or  y = And (pure (OrT  x) <|> pure (OrT y))
  Lit x /\ Lit y = And (pure (LitT x) <|> pure (LitT y))
  evalConj = evalConjDefault

instance Alternative f => Disjunctive (Expr f a) where
  F     \/ x     = x
  x     \/ F     = x
  T     \/ _     = T
  _     \/ T     = T
  Or  x \/ Or  y = Or (x <|> y)
  Or  x \/ And y = Or (x <|> pure (AndT y)) -- maybe we should swap <> args
  Or  x \/ Lit y = Or (x <|> pure (LitT y)) -- maybe we should swap <> args
  And x \/ Or  y = Or (pure (AndT x) <|> y)
  Lit x \/ Or  y = Or (pure (LitT x) <|> y)
  And x \/ Lit y = Or (pure (AndT x) <|> pure (LitT y))
  Lit x \/ And y = Or (pure (LitT x) <|> pure (AndT y))
  And x \/ And y = Or (pure (AndT x) <|> pure (AndT y))
  Lit x \/ Lit y = Or (pure (LitT x) <|> pure (LitT y))
  evalDisj = evalDisjDefault

negT :: Functor f => ExprT t f a -> ExprT (NEG t) f a
negT (OrT x)  = AndT (negT <$> x)
negT (AndT x) = OrT (negT <$> x)
negT (LitT l) = LitT (neg l)

instance (Functor f) => Negative (Expr f a) where
  neg T       = F
  neg F       = T
  neg (Or x)  = And (negT <$> x)
  neg (And x) = Or (negT <$> x)
  neg (Lit l) = Lit (neg l)

instance Alternative f => Implicative (Expr f a) where
  imply = implyDefault

instance Alternative f => Boolean (Expr f a) where
