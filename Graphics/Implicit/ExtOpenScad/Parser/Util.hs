-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright (C) 2016 Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

-- Allow us to use explicit foralls when writing function type declarations.
{-# LANGUAGE ExplicitForAll #-}

{-# LANGUAGE KindSignatures, FlexibleContexts #-}

module Graphics.Implicit.ExtOpenScad.Parser.Util (genSpace, pad, (*<|>), (?:), stringGS, padString, tryMany, variableSymb, patternMatcher) where

import Text.ParserCombinators.Parsec hiding (State)
import Text.Parsec.Prim (ParsecT, Stream)
import Data.Functor.Identity (Identity)
import Graphics.Implicit.ExtOpenScad.Definitions

-- white space, including tabs, newlines and comments
genSpace :: ParsecT [Char] u Identity [Char]
genSpace = many $
    oneOf " \t\n\r"
    <|> (try $ do
        _ <- string "//"
        _ <- many ( noneOf "\n")
        _ <- string "\n"
        return ' '
    ) <|> (try $ do
        _ <- string "/*"
        _ <- manyTill anyChar (try $ string "*/")
        return ' '
    )

pad :: forall b u. ParsecT [Char] u Identity b -> ParsecT [Char] u Identity b
pad parser = do
    _ <- genSpace
    a <- parser
    _ <- genSpace
    return a

infixr 1 *<|>
(*<|>) :: forall u a tok. GenParser tok u a -> ParsecT [tok] u Identity a -> ParsecT [tok] u Identity a
a *<|> b = try a <|> b

infixr 2 ?:
(?:) :: forall s u (m :: * -> *) a. String -> ParsecT s u m a -> ParsecT s u m a
l ?: p = p <?> l

stringGS :: [Char] -> ParsecT [Char] u Identity [Char]
stringGS (' ':xs) = do
    x'  <- genSpace
    xs' <- stringGS xs
    return (x' ++ xs')
stringGS (x:xs) = do
    x'  <- char x
    xs' <- stringGS xs
    return (x' : xs')
stringGS "" = return ""

padString :: String -> ParsecT [Char] u Identity String
padString s = do
    _ <- genSpace
    s' <- string s
    _ <- genSpace
    return s'

tryMany :: forall u a tok. [GenParser tok u a] -> ParsecT [tok] u Identity a
tryMany = (foldl1 (<|>)) . (map try)

variableSymb :: forall s u (m :: * -> *). Stream s m Char => ParsecT s u m [Char]
variableSymb = many1 (noneOf " ,|[]{}()+-*&^%#@!~`'\"\\/;:.,<>?=") <?> "variable"


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
        components <- patternMatcher `sepBy` (try $ genSpace >> char ',' >> genSpace)
        _ <- genSpace
        _ <- char ']'
        return $ ListP components
    )
