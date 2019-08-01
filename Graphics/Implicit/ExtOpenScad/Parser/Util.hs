-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright (C) 2016 Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

-- Allow us to use explicit foralls when writing function type declarations.
{-# LANGUAGE ExplicitForAll #-}

-- FIXME: required. why?
{-# LANGUAGE KindSignatures, FlexibleContexts #-}

module Graphics.Implicit.ExtOpenScad.Parser.Util ((*<|>), (?:), tryMany, patternMatcher, sourcePosition, number, variable, boolean, scadString, scadUndefined) where

import Prelude (String, Char, ($), foldl1, map, (.), return, (>>), Bool(True, False), read, (**), (*), (==), (++))

import Text.Parsec (SourcePos, (<|>), (<?>), try, char, sepBy, noneOf, string, many, digit, many1, optional, choice, option, oneOf)

import Text.Parsec.String (GenParser)

import qualified Text.Parsec as P (sourceLine, sourceColumn, sourceName)

import Text.Parsec.Prim (ParsecT)

import Data.Functor.Identity (Identity)

import Graphics.Implicit.ExtOpenScad.Definitions (Pattern(Wild, Name, ListP), SourcePosition(SourcePosition), Symbol(Symbol), Expr(LitE, Var), OVal(ONum, OString, OBool, OUndefined))

import Graphics.Implicit.Definitions (toFastℕ)

-- The lexer.
import Graphics.Implicit.ExtOpenScad.Parser.Lexer (matchIdentifier, matchTok, matchUndef, matchTrue, matchFalse, whiteSpace)

import Data.Kind (Type)

infixr 1 *<|>
(*<|>) :: forall u a tok. GenParser tok u a -> ParsecT [tok] u Identity a -> ParsecT [tok] u Identity a
a *<|> b = try a <|> b

infixr 2 ?:
(?:) :: forall s u (m :: Type -> Type) a. String -> ParsecT s u m a -> ParsecT s u m a
l ?: p = p <?> l

tryMany :: forall u a tok. [GenParser tok u a] -> ParsecT [tok] u Identity a
tryMany = foldl1 (<|>) . map try

-- | A pattern parser
patternMatcher :: GenParser Char st Pattern
patternMatcher =
    (do
        _ <- char '_'
        return Wild
    ) <|> {-( do
        a <- literal
        return $ \obj ->
            if obj == (a undefined)
            then Just (Map.empty)
            else Nothing
    ) <|> -} ( do
        symb <- matchIdentifier
        return $ Name (Symbol symb)
    ) <|> ( do
        _ <- matchTok '['
        components <- patternMatcher `sepBy` try (matchTok ',')
        _ <- matchTok ']'
        return $ ListP components
    )

-- expression parsers

-- | Parse a number.
number :: GenParser Char st Expr
number = ("number" ?:) $ do
  h <- choice
       [
         do
           a <- many1 digit
           b <- option "" (
             do
               c <- char '.' >> many1 digit
               return ("." ++ c)
             )
           return (a ++ b)
        ,
        do
          i <- char '.' >> many1 digit
          return ("0." ++ i)
        ]
  d <- option "0"
       (
         oneOf "eE" >> choice
         [do
             f <- char '-' >> many1 digit
             return ('-':f)
          ,
            optional (char '+') >> many1 digit
          ]
       )
  _ <- whiteSpace
  return . LitE $ ONum $ if d == "0"
                         then read h
                         else read h * (10 ** read d)

-- | Parse a variable reference.
--   NOTE: abused by the parser for function calls.
variable :: GenParser Char st Expr
variable = ("variable" ?:) $ do
  a <- matchIdentifier
  return (Var (Symbol a))

-- | Parse a true or false value.
boolean :: GenParser Char st Expr
boolean = ("boolean" ?:) $ do
  b  <-      (matchTrue  >> return True )
        *<|> (matchFalse >> return False)
  return . LitE $ OBool b

-- | Parse a quoted string.
--   FIXME: no \u unicode support?
scadString :: GenParser Char st Expr
scadString = ("string" ?:) $ do
  _ <- char '"'
  strlit <-  many $ (string "\\\"" >> return '\"')
               *<|> (string "\\n" >> return '\n')
               *<|> (string "\\r" >> return '\r')
               *<|> (string "\\t" >> return '\t')
               *<|> (string "\\\\" >> return '\\')
               *<|>  noneOf "\"\n"
  _ <- matchTok '"'
  return . LitE $ OString strlit

scadUndefined :: GenParser Char st Expr
scadUndefined = ("undefined" ?:) $ do
  _ <- matchUndef
  return . LitE $ OUndefined

sourcePosition :: SourcePos -> SourcePosition
sourcePosition pos = SourcePosition (toFastℕ $ P.sourceLine pos) (toFastℕ $ P.sourceColumn pos) (P.sourceName pos)
