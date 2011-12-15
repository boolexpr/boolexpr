module Data.Boolean.Parser
  (-- * Parsing function
   parseBoolean
   -- * Language definition and components
  ,languageDef
  ,lexer
  ,identifier
  ,whiteSpace
  ,symbol
  )
where

import Control.Monad
import Control.Applicative hiding ((<|>))
import Data.Boolean
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P

{- | Parse a search query as a boolean expression tree using the following grammar.
     Note that this parser is parameterized over the parser of query simple
     terms (literals).

@
  bt ::= bt AND bt
       | bt bt -- same as AND
       | bt OR bt
       | - bt
       | ( bt )
       | const
   const ::= \<given as argument\>
@
-}
parseBoolean :: (Applicative f, Boolean (f a)) => CharParser st a -> CharParser st (f a)
parseBoolean parseConst = disj
   where disj   = conj   `chainl1` orOp
         conj   = factor `chainl1` andOp
         factor = parens disj <|>
                  (((symbol "-" >> return neg) <|> return id) `ap` (pure `fmap` parseConst))

         andOp = (/\) <$ option "" (symbol "AND")
         orOp  = (\/) <$ symbol "OR"

-- | Underlying lexer of 'languageDef'
lexer :: P.TokenParser st
lexer = P.makeTokenParser languageDef

-- | Shorthand for 'P.parens lexer'.
parens :: CharParser st a -> CharParser st a
parens = P.parens lexer

-- | Shorthand for 'P.symbol' 'lexer'.
symbol :: String -> CharParser st String
symbol = P.symbol lexer

-- | Shorthand for 'P.whiteSpace' 'lexer'.
whiteSpace :: CharParser st ()
whiteSpace = P.whiteSpace lexer

-- | Shorthand for 'P.identifier' 'lexer'.
identifier :: CharParser st String
identifier = P.identifier lexer

wordLetter :: CharParser st Char
wordLetter = alphaNum <|> oneOf "_:;`,~@.!#$%^&*=+?|\\{}[]<>"

-- | Basic language definition for search queries.
-- Reserved names are @\"AND\"@ @\"OR\"@ and @\"-\"@.
-- Identifiers accepts almost every ASCII sequences without blanks nor @\'-\'@.
languageDef :: P.LanguageDef st
languageDef = P.LanguageDef
               { P.commentStart   = ""
               , P.commentEnd     = ""
               , P.commentLine    = ""
               , P.nestedComments = True
               , P.identStart     = wordLetter
               , P.identLetter    = wordLetter <|> char '-'
               , P.opStart        = mzero
               , P.opLetter       = mzero
               , P.reservedOpNames= []
               , P.reservedNames  = ["AND", "OR", "-"]
               , P.caseSensitive  = True
               }
