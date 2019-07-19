-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright 2014 2015 2016, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

-- Allow us to use shorter forms of Var and Name.
{-# LANGUAGE PatternSynonyms #-}

-- A parser for a numeric expressions.
module Graphics.Implicit.ExtOpenScad.Parser.Expr(expr0) where

import Prelude (Char, Maybe(Nothing, Just), String, ($), (.), (>>), return, Bool(True, False), read, (++), (*), (**), id, foldl, map, foldl1, unzip, tail, zipWith3, foldr, (==), length, mod)

-- The parsec parsing library.
import Text.Parsec (oneOf, string, many1, digit, char, many, noneOf, sepBy, sepBy1, optionMaybe, try, option, optional, choice)

import Text.Parsec.String (GenParser)

import Graphics.Implicit.ExtOpenScad.Definitions (Expr(LamE, LitE, ListE, (:$)), OVal(ONum, OString, OBool, OUndefined), collector, Symbol(Symbol))

import qualified Graphics.Implicit.ExtOpenScad.Definitions as GIED (Expr(Var), Pattern(Name))

import Graphics.Implicit.ExtOpenScad.Parser.Util ((?:), (*<|>))

-- The lexer.
import Graphics.Implicit.ExtOpenScad.Parser.Lexer (whiteSpace, matchTrue, matchFalse, matchLet, matchUndef, matchTok, matchColon, matchComma, surroundedBy, matchIdentifier)

-- Let us use the old syntax when defining Vars and Names.
pattern Var :: String -> Expr
pattern Var  s = GIED.Var  (Symbol s)
pattern Name :: String -> GIED.Pattern
pattern Name n = GIED.Name (Symbol n)

-- | Parse a variable reference.
--   NOTE: abused by the parser for function calls.
variable :: GenParser Char st Expr
variable = ("variable" ?:) $ do
  a <- matchIdentifier
  return (Var a)

-- | Parse a true or false value.
boolean :: GenParser Char st Expr
boolean = ("boolean" ?:) $ do
  b  <-      (matchTrue  >> return True )
        *<|> (matchFalse >> return False)
  return . LitE $ OBool b

-- | Parse a number.
number :: GenParser Char st Expr
number = ("number" ?:) $ do
  h <- choice
       [(
           do
             a <- many1 digit
             b <- option "" (
               do
                 c <- char '.' >> many1 digit
                 return ("." ++ c)
               )
             return (a ++ b)
        ),
        ( do
            i <- char '.' >> many1 digit
            return ("0." ++ i)
        )]
  d <- option "0"
       (
         oneOf "eE" >> choice
         [( do
              f <- char '-' >> many1 digit
              return ("-" ++ f)
          ),
          (
            (optional $ char '+') >> many1 digit
          )]
       )
  _ <- whiteSpace
  return . LitE $ ONum $ if d == "0"
                         then read (h)
                         else read (h) * (10 ** read d)

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

letExpr :: GenParser Char st Expr
letExpr = "let expression" ?: do
  _ <- matchLet >> matchTok '('
  bindingPairs <- sepBy ( do
    boundName <- matchIdentifier
    boundExpr <- matchTok '=' >> expr0
    return $ ListE [Var boundName, boundExpr])
    (matchComma)
  _ <- matchTok ')'
  expr <- expr0
  let bindLets (ListE [Var boundName, boundExpr]) nestedExpr = (LamE [Name boundName] nestedExpr) :$ [boundExpr]
      bindLets _ e = e
  return $ foldr bindLets expr bindingPairs

-- We represent the priority or 'fixity' of different types of expressions
-- by the ExprIdx argument, with A1 as the highest.

expr0 :: GenParser Char st Expr
expr0 = exprN A1

-- what state in the expression parser tree we are inside of.
data ExprIdx = A1 | A2 | A3 | A4 | A5 | A6 | A7 | A8 | A9 | A10 | A11 | A12 | A13

exprN :: ExprIdx -> GenParser Char st Expr

exprN A13 =
       number
  *<|> boolean
  *<|> scadString
  *<|> scadUndefined

-- | Parse parentheses, lists, vectors, and vector/list generators.
exprN A12 =
         letExpr
    *<|> "bracketed expression" ?:
         -- eg. ( 1 + 5 )
         surroundedBy '(' expr0 ')'
    *<|> "vector/list" ?: do
            -- eg. [ 3, a, a+1, b, a*b] or ( 1, 2, 3)
            o <- oneOf "[("
            _ <- whiteSpace        
            exprs <- sepBy expr0 matchComma
            _ <- if (o == '[')
                 then matchTok ']'
                 else matchTok ')'
            return $ ListE exprs
    *<|> "vector/list generator" ?: do
        -- eg.  [ a : 1 : a + 10 ]
        _ <- matchTok '['
        expr1 <- expr0
        _     <- matchColon
        exprs <- (do
                    expr2 <- expr0
                    expr3 <- optionMaybe (matchColon >> expr0)
                    return $ case expr3 of
                      (Just n)  -> [expr2, n]
                      (Nothing) -> [expr2]
                 )
        _ <- matchTok ']'
        return $ collector "list_gen" ([expr1] ++ exprs)
    *<|> exprN A13

-- | parse operations that start with a variable name, including variable reference..
exprN A11 =
    do
        obj <- variable
        args <- option [] (
          "function application" ?: do
              args <- surroundedBy '(' (sepBy expr0 matchComma) ')'
              return $ [\f -> f :$ args]
          )
        mods <- many (
               "list indexing" ?: do
                   i <- surroundedBy '[' expr0 ']'
                   return $ \l -> Var "index" :$ [l, i]
          *<|> "list splicing" ?: do
                   _     <- matchTok '['
                   start <- optionMaybe expr0
                   _     <- matchColon
                   end   <- optionMaybe expr0
                   _     <- matchTok ']'
                   return $ case (start, end) of
                              (Nothing, Nothing) -> id
                              (Just s,  Nothing)  -> \l -> Var "splice" :$ [l, s, LitE OUndefined ]
                              (Nothing, Just e )  -> \l -> Var "splice" :$ [l, LitE $ ONum 0, e]
                              (Just s,  Just e )  -> \l -> Var "splice" :$ [l, s, e]
                 )
        return $ foldl (\a b -> b a) obj (args ++ mods)
    *<|> exprN A12

-- match a leading (+) or (-) operator.
exprN A10 =
    "negation" ?: do
        expr <- matchTok '-' >> exprN A11
        return $ Var "negate" :$ [expr]
    *<|> do
        matchTok '+' >> exprN A11
    *<|> exprN A11

-- match power-of (^) operator.
exprN A9 =
    "exponentiation" ?: do
        a <- exprN A10
        b <- matchTok '^' >> exprN A9
        return $ Var "^" :$ [a,b]
    *<|> exprN A10

-- match sequences of multiplication and division.
exprN A8 =
    "multiplication/division" ?: do
        -- outer list is multiplication, inner division.
        -- eg. "1*2*3/4/5*6*7/8"
        --     [[1],[2],[3,4,5],[6],[7,8]]
        exprs <- sepBy1
            (sepBy1 (exprN A9) (try $ matchTok '/'))
            (try $ matchTok '*')
        let div' a b = Var "/" :$ [a, b]
        return . collector "*" $ map (foldl1 div') exprs
    *<|> exprN A9

-- match remainder (%) operator.
exprN A7 =
    "modulo" ?: do
        exprs <- sepBy1 (exprN A8) (try $ matchTok '%')
        let mod' a b = Var "%" :$ [a, b]
        return $ foldl1 mod' exprs
    *<|> exprN A8

-- match string addition (++) operator.
exprN A6 =
    "append" ?: do
        exprs <- sepBy1 (exprN A7) (try $ string "++" >> whiteSpace)
        return $ collector "++" exprs
    *<|> exprN A7

-- match sequences of addition and subtraction.
exprN A5 =
    "addition/subtraction" ?: do
        -- Similar to multiply & divide
        -- eg. "1+2+3-4-5+6-7"
        --     [[1],[2],[3,4,5],[6,7]]
        exprs <- sepBy1
            (sepBy1 (exprN A6) (try $ matchTok '-'))
            (try $ matchTok '+')
        let sub a b = Var "-" :$ [a, b]
        return . collector "+" $ map (foldl1 sub) exprs
    *<|> exprN A6

-- match comparison operators.
exprN A4 =
    do
        firstExpr <- exprN A5
        otherComparisonsExpr <- many $ do
            comparisonSymb <-
                   string "=="
              *<|> string "!="
              *<|> string ">="
              *<|> string "<="
              *<|> string ">"
              *<|> string "<"
            _ <- whiteSpace
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

-- | Match the logical negation operator.
exprN A3 =
    "logical-not" ?: do
        a <- many1 $ matchTok '!'
        b <- exprN A4
        return $ if ((length a) `mod` 2==0)
                 then b
                 else Var "!" :$ [b]
    *<|> exprN A4

-- | Match the logical And and Or (&&,||) operators.
exprN A2 =
    "logical and/or" ?: do
        a <- exprN A3
        symb <-      string "&&"
                *<|> string "||"
        _ <- whiteSpace
        b <- exprN A2
        return $ Var symb :$ [a,b]
    *<|> exprN A3

-- match the ternary (1?2:3) operator.
exprN A1 =
    "ternary" ?: do
        a <- exprN A2
        _ <- matchTok '?'
        b <- exprN A1
        _ <- matchColon
        c <- exprN A1
        return $ Var "?" :$ [a,b,c]
    *<|> exprN A2

