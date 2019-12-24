-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright (C) 2016 Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

-- Allow us to use explicit foralls when writing function type declarations.
{-# LANGUAGE ExplicitForAll #-}

-- FIXME: required. why?
{-# LANGUAGE KindSignatures, FlexibleContexts #-}

module Graphics.Implicit.ExtOpenScad.Parser.Util ((*<|>), (?:), tryMany, patternMatcher, sourcePosition, number, variable, boolean, scadString, scadUndefined) where

import Prelude (either, id, String, Char, ($), foldl1, map, (.), pure, (*>), Bool(True, False), read, (**), (*), (==), (<>), (<$>), (<$), fromInteger)

import Text.Parsec (SourcePos, (<|>), (<?>), try, char, sepBy, noneOf, string, many, digit, many1, option, oneOf, between)

import Text.Parsec.String (GenParser)

import qualified Text.Parsec as P (sourceLine, sourceColumn, sourceName)

import Text.Parsec.Prim (ParsecT)

import Text.Parsec.Token (naturalOrFloat, integer)

import Data.Functor.Identity (Identity)

import Graphics.Implicit.ExtOpenScad.Definitions (Pattern(Wild, Name, ListP), SourcePosition(SourcePosition), Symbol(Symbol), Expr(LitE, Var), OVal(ONum, OString, OBool, OUndefined))

import Graphics.Implicit.Definitions (toFastℕ)

-- The lexer.
import Graphics.Implicit.ExtOpenScad.Parser.Lexer (matchIdentifier, matchTok, matchUndef, matchTrue, matchFalse, whiteSpace, surroundedBy, matchComma)

import Data.Functor (($>))

import Data.Kind (Type)

import Graphics.Implicit.ExtOpenScad.Parser.Lexer (lexer)

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
patternMatcher = "pattern" ?:
          (Wild <$ char '_')
      <|> ( Name . Symbol <$> matchIdentifier)
      <|> ( ListP <$> surroundedBy '[' (patternMatcher `sepBy` matchComma) ']' )

-- expression parsers

-- | Parse a number.
number :: GenParser Char st Expr
number = ("number" ?:) $ do
  n <- try (either fromInteger id <$> naturalOrFloat lexer) <|> do
    h <- ("0."<>) <$> (char '.' *> many1 digit)
    d <- option 0 $ oneOf "eE" *> integer lexer
    pure $ if d == 0
      then read h
      else read h * (10 ** fromInteger d)
  _ <- whiteSpace
  pure . LitE $ ONum n 

-- | Parse a variable reference.
--   NOTE: abused by the parser for function calls.
variable :: GenParser Char st Expr
variable = "variable" ?:
  Var . Symbol <$> matchIdentifier

-- | Parse a true or false value.
boolean :: GenParser Char st Expr
boolean = "boolean" ?:
  LitE . OBool <$> (matchTrue $> True <|> matchFalse $> False)

-- | Parse a quoted string.
--   FIXME: no \u unicode support?
scadString :: GenParser Char st Expr
scadString = "string" ?: LitE . OString <$>
    between
      (char '"')
      (matchTok '"')
      (many $
        (string "\\\"" $> '\"') *<|>
        (string "\\n"  $> '\n') *<|>
        (string "\\r"  $> '\r') *<|>
        (string "\\t"  $> '\t') *<|>
        (string "\\\\" $> '\\') *<|>
        noneOf "\"\n"
      )

scadUndefined :: GenParser Char st Expr
scadUndefined = "undefined" ?:
  LitE OUndefined <$ matchUndef

sourcePosition :: SourcePos -> SourcePosition
sourcePosition pos = SourcePosition (toFastℕ $ P.sourceLine pos) (toFastℕ $ P.sourceColumn pos) (P.sourceName pos)
