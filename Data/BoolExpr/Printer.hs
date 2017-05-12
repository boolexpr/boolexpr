module Data.BoolExpr.Printer
  (-- * Printer
  boolExprPrinter
  )
where

import Data.BoolExpr

-- | Printer
boolExprPrinter :: (a -> ShowS) -> BoolExpr a -> ShowS
boolExprPrinter f = go
  where
    go (BAnd a b) = paren $ go a . text " AND " . go b
    go (BOr  a b) = paren $ go a . text " OR "  . go b
    go (BNot a)   = text "-" . paren (go a)
    go  BTrue     = text "TRUE" -- not in the parser
    go  BFalse    = text "FALSE" -- not in the parser
    go (BConst c) = pConst c

    pConst (Positive c) = f c
    pConst (Negative c) = text "-" . f c

    paren = showParen True
    text  = showString
