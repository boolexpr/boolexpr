# boolexpr
Haskell library for Boolean expressions with various representations and search queries.

## Query parser

in ghci: `runParser (parseBoolExpr identifier) () "" "(a OR b) AND (n OR m) NOT (x OR y)"`

Right (BAnd (BAnd (BOr (BConst "a") (BConst "b")) (BOr (BConst "n") (BConst "m"))) (BNot (BOr (BConst "x") (BConst "y"))))

it :: Either ParseError (BoolExpr String)

## Matching example

### Import and Functions declarations
```
import Data.List (isInfixOf)
matchingDoc :: Doc -> BoolExpr String -> Bool
matchingDoc doc = evalBoolExpr (hasWord doc)

```

### Example query 1
```
let Right query1 = runParser (parseBoolExpr identifier) () "" "Haskell"

matchingString "I really like Haskell" query1 == True
```

### Example query 2

```
let Right query2 = runParser (parseBoolExpr identifier) () "" "Haskell NOT I"

matchingString "I really like Haskell" query2 == False

matchingString "You really like Haskell" query2 == True
```




## Boolean Tree manipulation


