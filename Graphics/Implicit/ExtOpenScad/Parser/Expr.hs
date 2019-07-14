-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright 2014 2015 2016, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

-- Allow us to use shorter forms of Var and Name.
{-# LANGUAGE PatternSynonyms #-}

-- A parser for a numeric expressions.
module Graphics.Implicit.ExtOpenScad.Parser.Expr(expr0) where

import Prelude (Char, Maybe(Nothing, Just), String, fmap, ($), (.), (>>), return, Bool(True, False), read, (++), (*), id, foldl, map, foldl1, unzip, tail, zipWith3, foldr)

-- for a 'power' operator, from our math library.
import Graphics.Implicit.Definitions (powℝ)

-- The parsec parsing library.
import Text.Parsec (string, many1, digit, char, many, noneOf, sepBy, sepBy1, optionMaybe, try, option, optional, choice)

import Text.Parsec.String (GenParser)

import Graphics.Implicit.ExtOpenScad.Definitions (Expr(LamE, LitE, ListE, (:$)), OVal(ONum, OString, OBool, OUndefined), collector, Symbol(Symbol))

import qualified Graphics.Implicit.ExtOpenScad.Definitions as GIED (Expr(Var), Pattern(Name))

import Graphics.Implicit.ExtOpenScad.Parser.Util (variableSymb, (?:), (*<|>), genSpace, padString, padChar)

-- Let us use the old syntax when defining Vars and Names.
pattern Var :: String -> Expr
pattern Var  s = GIED.Var  (Symbol s)
pattern Name :: String -> GIED.Pattern
pattern Name n = GIED.Name (Symbol n)

variable :: GenParser Char st Expr
variable = fmap Var variableSymb

-- | Parse a true or false value.
boolean :: GenParser Char st Expr
boolean = ("boolean" ?:) $ do
  b  <-      (string "true"  >> return True )
        *<|> (string "false" >> return False)
  return . LitE $ OBool b

literal :: GenParser Char st Expr
literal = ("literal" ?:) $
    "number" ?:
         do
            a <- many1 digit
            b <- option "" (
              do
                _ <- char '.'
                c <- many1 digit
                return ("." ++ c)
              )
            d <- option "0" (
              do
                _ <- char 'e'
                exponent <- choice [
                  ( do
                      e <- char '-'
                      f <- many1 digit
                      return (e : f)
                  ),
                  ( do
                      _ <- optional $ char '+'
                      g <- many1 digit
                      return g
                  )]
                return exponent
              )
            return . LitE $ ONum $ read (a ++ b) * (10 `powℝ` read d)
     *<|> boolean
     *<|> "string" ?: do
        _ <- char '"'
        strlit <-  many $ (string "\\\"" >> return '\"')
                     *<|> (string "\\n" >> return '\n')
                     *<|> (string "\\r" >> return '\r')
                     *<|> (string "\\t" >> return '\t')
                     *<|> (string "\\\\" >> return '\\')
                      -- FIXME: no \u unicode support?
                     *<|> noneOf "\"\n"
        _ <- char '"'
        return . LitE $ OString strlit

letExpr :: GenParser Char st Expr
letExpr = "let expression" ?: do
  _ <- string "let"
  _ <- padChar '('
  bindingPairs <- sepBy ( do
    _ <- genSpace
    boundName <- variableSymb
    _ <- padChar '='
    boundExpr <- expr0
    return $ ListE [Var boundName, boundExpr])
    (char ',')
  _ <- char ')'
  expr <- expr0
  let bindLets (ListE [Var boundName, boundExpr]) nestedExpr = (LamE [Name boundName] nestedExpr) :$ [boundExpr]
      bindLets _ e = e
  return $ foldr bindLets expr bindingPairs

-- We represent the priority or 'fixity' of different types of expressions
-- by the ExprIdx argument, with A0 as the highest.

expr0 :: GenParser Char st Expr
expr0 = exprN A0

-- what state in the expression parser tree we are inside of.
data ExprIdx = A0 | A1 | A2 | A3 | A4 | A5 | A6 | A7 | A8 | A9 | A10 | A11 | A12

exprN :: ExprIdx -> GenParser Char st Expr

exprN A12 =
         literal
    *<|> letExpr
    *<|> variable
    *<|> "bracketed expression" ?: do
        -- eg. ( 1 + 5 )
        _ <- char '('
        expr <- expr0
        _ <- char ')'
        return expr
    *<|> "vector/list" ?: (
        do
            -- eg. [ 3, a, a+1, b, a*b ]
            _ <- char '['
            exprs <- sepBy expr0 (char ',' )
            _ <- char ']'
            return $ ListE exprs
        *<|> do
            -- eg. ( 1,2,3 )
            _ <- char '('
            exprs <- sepBy expr0 (char ',' )
            _ <- char ')'
            return $ ListE exprs
        )
    *<|> "vector/list generator" ?: do
        -- eg.  [ a : 1 : a + 10 ]
        _ <- char '['
        exprs <- sepBy expr0 (char ':' )
        _ <- char ']'
        return $ collector "list_gen" exprs

exprN A11 =
    do
        obj <- exprN A12
        _ <- genSpace
        mods <- many1 (
            "function application" ?: do
                _ <- padChar '('
                args <- sepBy expr0 (padChar ',')
                _ <- padChar ')'
                return $ \f -> f :$ args
            *<|> "list indexing" ?: do
                _ <- padChar '['
                i <- expr0
                _ <- padChar ']'
                return $ \l -> Var "index" :$ [l, i]
            *<|> "list splicing" ?: do
                _ <- padChar '['
                start <- optionMaybe expr0
                _ <- padChar ':'
                end   <- optionMaybe expr0
                _ <- padChar ']'
                return $ case (start, end) of
                    (Nothing, Nothing) -> id
                    (Just s,  Nothing)  -> \l -> Var "splice" :$ [l, s, LitE OUndefined ]
                    (Nothing, Just e )  -> \l -> Var "splice" :$ [l, LitE $ ONum 0, e]
                    (Just s,  Just e )  -> \l -> Var "splice" :$ [l, s, e]
            )
        return $ foldl (\a b -> b a) obj mods
    *<|> exprN A12

-- match a leading (+) or (-) operator.
exprN A10 =
    "negation" ?: do
        _ <- padChar '-'
        expr <- exprN A11
        return $ Var "negate" :$ [expr]
    *<|> do
        _ <- padChar '+'
        exprN A11
    *<|> exprN A11

-- match power-of (^) operator.
exprN A9 =
    "exponentiation" ?: do
        a <- exprN A10
        _ <- padChar '^'
        b <- exprN A9
        return $ Var "^" :$ [a,b]
    *<|> exprN A10

-- match sequences of multiplication and division.
exprN A8 =
    "multiplication/division" ?: do
        -- outer list is multiplication, inner division.
        -- eg. "1*2*3/4/5*6*7/8"
        --     [[1],[2],[3,4,5],[6],[7,8]]
        exprs <- sepBy1
            (sepBy1 (exprN A9) (try $ padChar '/' ))
            (try $ padChar '*' )
        let div' a b = Var "/" :$ [a, b]
        return . collector "*" $ map (foldl1 div') exprs
    *<|> exprN A9

-- match remainder (%) operator.
exprN A7 =
    "modulo" ?: do
        exprs <- sepBy1 (exprN  A8) (try $ padChar '%')
        let mod' a b = Var "%" :$ [a, b]
        return $ foldl1 mod' exprs
    *<|> exprN A8

-- match string addition (++) operator.
exprN A6 =
    "append" ?: do
        exprs <- sepBy1 (exprN A7) (try $ padString "++")
        return $ collector "++" exprs
    *<|> exprN A7

-- match sequences of addition and subtraction.
exprN A5 =
    "addition/subtraction" ?: do
        -- Similar to multiply & divide
        -- eg. "1+2+3-4-5+6-7"
        --     [[1],[2],[3,4,5],[6,7]]
        exprs <- sepBy1
            (sepBy1 (exprN A6) (try $ padChar '-' ))
            (try $ padChar '+' )
        let sub a b = Var "-" :$ [a, b]
        return . collector "+" $ map (foldl1 sub) exprs
    *<|> exprN A6

-- match comparison operators.
exprN A4 =
    do
        firstExpr <- exprN A5
        otherComparisonsExpr <- many $ do
            comparisonSymb <-
                     padString "=="
                *<|> padString "!="
                *<|> padString ">="
                *<|> padString "<="
                *<|> padString ">"
                *<|> padString "<"
            expr <- exprN A5
            return (Var comparisonSymb, expr)
        let
            (comparisons, otherExprs) = unzip otherComparisonsExpr
            exprs = firstExpr:otherExprs
        return $ case comparisons of
            []  -> firstExpr
            [x] -> x :$ exprs
            _   -> collector "all" $ zipWith3 (\c e1 e2 -> c :$ [e1,e2]) comparisons exprs (tail exprs)
    *<|> exprN A5

-- match the logical negation operator.
exprN A3 =
    "logical-not" ?: do
        _ <- padChar '!'
        a <- exprN A4
        return $ Var "!" :$ [a]
    *<|> exprN A4

-- match the logical And and Or (&&,||) operators.
exprN A2 =
    "logical and/or" ?: do
        a <- exprN A3
        symb <-      padString "&&"
                *<|> padString "||"
        b <- exprN A2
        return $ Var symb :$ [a,b]
    *<|> exprN A3

-- match the ternary (1?2:3) operator.
exprN A1 =
    "ternary" ?: do
        a <- exprN A2
        _ <- padChar '?'
        b <- exprN A1
        _ <- padChar ':'
        c <- exprN A1
        return $ Var "?" :$ [a,b,c]
    *<|> exprN A2

-- Match and throw away any white space around an expression.
exprN A0 =
    do
        _ <- genSpace
        expr <- exprN A1
        _ <- genSpace
        return expr
    *<|> exprN A1

