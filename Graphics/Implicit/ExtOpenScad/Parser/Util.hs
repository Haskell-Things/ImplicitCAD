-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright (C) 2016 Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

-- Allow us to use explicit foralls when writing function type declarations.
{-# LANGUAGE ExplicitForAll #-}

-- FIXME: required. why?
{-# LANGUAGE KindSignatures, FlexibleContexts #-}

module Graphics.Implicit.ExtOpenScad.Parser.Util ((*<|>), (?:), tryMany, patternMatcher, sourcePosition) where

import Prelude (String, Char, ($), foldl1, map, (>>), (.), return)

import Text.Parsec (SourcePos, (<|>), (<?>), try, char, sepBy)

import Text.Parsec.String (GenParser)

import qualified Text.Parsec as P (sourceLine, sourceColumn, sourceName)

import Text.Parsec.Prim (ParsecT)

import Data.Functor.Identity (Identity)

import Graphics.Implicit.ExtOpenScad.Definitions (Pattern(Wild, Name, ListP), SourcePosition(SourcePosition), Symbol(Symbol))

import Graphics.Implicit.Definitions (toFastℕ)

-- The lexer.
import Graphics.Implicit.ExtOpenScad.Parser.Lexer (whiteSpace, matchIdentifier, matchTok)

import Data.Kind (Type)

infixr 1 *<|>
(*<|>) :: forall u a tok. GenParser tok u a -> ParsecT [tok] u Identity a -> ParsecT [tok] u Identity a
a *<|> b = try a <|> b

infixr 2 ?:
(?:) :: forall s u (m :: Type -> Type) a. String -> ParsecT s u m a -> ParsecT s u m a
l ?: p = p <?> l

tryMany :: forall u a tok. [GenParser tok u a] -> ParsecT [tok] u Identity a
tryMany = foldl1 (<|>) . map try

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
        components <- patternMatcher `sepBy` try (char ',' >> whiteSpace)
        _ <- matchTok ']'
        return $ ListP components
    )

sourcePosition :: SourcePos -> SourcePosition
sourcePosition pos = SourcePosition (toFastℕ $ P.sourceLine pos) (toFastℕ $ P.sourceColumn pos) (P.sourceName pos)
