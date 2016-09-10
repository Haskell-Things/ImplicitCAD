-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright (C) 2016 Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

-- Allow us to use explicit foralls when writing function type declarations.
{-# LANGUAGE ExplicitForAll #-}

-- FIXME: required. why?
{-# LANGUAGE KindSignatures, FlexibleContexts #-}

module Graphics.Implicit.ExtOpenScad.Parser.Util (genSpace, pad, (*<|>), (?:), stringGS, padString, tryMany, variableSymb, patternMatcher, sourcePosition) where

import Prelude (String, Char, ($), (++), foldl1, map, (>>), (.), return)

import Text.ParserCombinators.Parsec (GenParser, SourcePos, many, oneOf, noneOf, (<|>), try, string, manyTill, anyChar, (<?>), char, many1, sepBy)

import qualified Text.ParserCombinators.Parsec as P (sourceLine, sourceColumn, sourceName)

import Text.Parsec.Prim (ParsecT, Stream)

import Data.Functor.Identity (Identity)

import Graphics.Implicit.ExtOpenScad.Definitions (Pattern(Wild, Name, ListP), SourcePosition(SourcePosition))

import Graphics.Implicit.Definitions (toFastℕ)

import Data.Kind (Type)

-- | Consume white space, including tabs, newlines and comments.
genSpace :: ParsecT String u Identity String
genSpace = many $
    oneOf " \t\n\r"
    <|> try ( do
        _ <- string "//"
        _ <- many ( noneOf "\n")
        return ' '
    ) <|> try ( do
        _ <- string "/*"
        _ <- manyTill anyChar (try $ string "*/")
        return ' '
    )

-- a padded ... parser?
pad :: ParsecT String u Identity b -> ParsecT String u Identity b
pad parser = do
    _ <- genSpace
    a <- parser
    _ <- genSpace
    return a

infixr 1 *<|>
(*<|>) :: forall u a tok. GenParser tok u a -> ParsecT [tok] u Identity a -> ParsecT [tok] u Identity a
a *<|> b = try a <|> b

infixr 2 ?:
(?:) :: forall s u (m :: Type -> Type) a. String -> ParsecT s u m a -> ParsecT s u m a
l ?: p = p <?> l

stringGS :: String -> ParsecT String u Identity String
stringGS (' ':xs) = do
    x'  <- genSpace
    xs' <- stringGS xs
    return (x' ++ xs')
stringGS (x:xs) = do
    x'  <- char x
    xs' <- stringGS xs
    return (x' : xs')
stringGS "" = return ""

-- a padded string
padString :: String -> ParsecT String u Identity String
padString s = do
    _ <- genSpace
    s' <- string s
    _ <- genSpace
    return s'

tryMany :: forall u a tok. [GenParser tok u a] -> ParsecT [tok] u Identity a
tryMany = foldl1 (<|>) . map try

variableSymb :: forall s u (m :: Type -> Type). Stream s m Char => ParsecT s u m String
variableSymb = many1 (noneOf " ,|[]{}()+-*&^%#@!~`'\"\\/;:.,<>?=") <?> "variable"
{-# INLINABLE variableSymb #-}

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
        symb <- variableSymb
        return $ Name symb
    ) <|> ( do
        _ <- char '['
        _ <- genSpace
        components <- patternMatcher `sepBy` try (genSpace >> char ',' >> genSpace)
        _ <- genSpace
        _ <- char ']'
        return $ ListP components
    )

sourcePosition :: SourcePos -> SourcePosition
sourcePosition pos = SourcePosition (toFastℕ $ P.sourceLine pos) (toFastℕ $ P.sourceColumn pos) (P.sourceName pos)
