-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright 2014 2015 2016, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

module Graphics.Implicit.ExtOpenScad.Parser.Expr where

import Graphics.Implicit.Definitions
import Text.ParserCombinators.Parsec  hiding (State)
import Graphics.Implicit.ExtOpenScad.Definitions
import Graphics.Implicit.ExtOpenScad.Parser.Util

variable :: GenParser Char st Expr
variable = fmap Var variableSymb

literal :: GenParser Char st Expr
literal = ("literal" ?:) $
    "boolean" ?: do
        b  <-      (string "true"  >> return True )
              *<|> (string "false" >> return False)
        return $ LitE $ OBool b
    *<|> "number" ?: (
        do
            a <- many1 digit
            _ <- char '.'
            b <- many digit
            return $ LitE $ ONum (read (a ++ "." ++ b) :: ℝ)
        *<|>  do
            a <- many1 digit
            return $ LitE $ ONum (read a :: ℝ)
        )
    *<|> "string" ?: do
        _ <- string "\""
        strlit <-  many $ (string "\\\"" >> return '\"')
                     *<|> (string "\\n" >> return '\n')
                     *<|> ( noneOf "\"\n")
        _ <- string "\""
        return $ LitE $ OString strlit

-- We represent the priority or 'fixity' of different types of expressions
-- by the Int argument

expr0 :: GenParser Char st Expr
expr0 = exprN A0

-- what state in the expression parser tree we are inside of.
data ExprIdx = A0 | A1 | A2 | A3 | A4 | A5 | A6 | A7 | A8 | A9 | A10 | A11 | A12

exprN :: ExprIdx -> GenParser Char st Expr

exprN A12 =
         literal
    *<|> variable
    *<|> "bracketed expression" ?: do
        -- eg. ( 1 + 5 )
        _ <- string "("
        expr <- expr0
        _ <- string ")"
        return expr
    *<|> "vector/list" ?: (
        do
            -- eg. [ 3, a, a+1, b, a*b ]
            _ <- string "["
            exprs <- sepBy expr0 (char ',' )
            _ <- string "]"
            return $ ListE exprs
        *<|> do
            -- eg. ( 1,2,3 )
            _ <- string "("
            exprs <- sepBy expr0 (char ',' )
            _ <- string ")"
            return $ ListE exprs
        )
    *<|> "vector/list generator" ?: do
        -- eg.  [ a : 1 : a + 10 ]
        _ <- string "["
        exprs <- sepBy expr0 (char ':' )
        _ <- string "]"
        return $ collector "list_gen" exprs

exprN A11 =
    do
        obj <- exprN $ A12
        _ <- genSpace
        mods <- many1 (
            "function application" ?: do
                _ <- padString "("
                args <- sepBy expr0 (padString ",")
                _ <- padString ")"
                return $ \f -> f :$ args
            *<|> "list indexing" ?: do
                _ <- padString "["
                i <- expr0
                _ <- padString "]"
                return $ \l -> Var "index" :$ [l, i]
            *<|> "list splicing" ?: do
                _ <- padString "["
                start <- optionMaybe expr0
                _ <- padString ":"
                end   <- optionMaybe expr0
                _ <- padString "]"
                return $ case (start, end) of
                    (Nothing, Nothing) -> id
                    (Just s,  Nothing)  -> \l -> Var "splice" :$ [l, s, LitE OUndefined ]
                    (Nothing, Just e )  -> \l -> Var "splice" :$ [l, LitE $ ONum 0, e]
                    (Just s,  Just e )  -> \l -> Var "splice" :$ [l, s, e]
            )
        return $ foldl (\a b -> b a) obj mods
    *<|> (exprN $ A12 )

exprN A10 =
    "negation" ?: do
        _ <- padString "-"
        expr <- exprN $ A11
        return $ Var "negate" :$ [expr]
    *<|> do
        _ <- padString "+"
        expr <- exprN $ A11
        return expr
    *<|> exprN (A11)

exprN A9 =
    "exponentiation" ?: do
        a <- exprN $ A10
        _ <- padString "^"
        b <- exprN A9
        return $ Var "^" :$ [a,b]
    *<|> exprN (A10)

exprN A8 =
    "multiplication/division" ?: do
        -- outer list is multiplication, inner division.
        -- eg. "1*2*3/4/5*6*7/8"
        --     [[1],[2],[3,4,5],[6],[7,8]]
        exprs <- sepBy1
            (sepBy1 (exprN $ A9) (try $ padString "/" ))
            (try $ padString "*" )
        let div' a b = Var "/" :$ [a, b]
        return $ collector "*" $ map (foldl1 div') exprs
    *<|> exprN (A9)

exprN A7 =
    "modulo" ?: do
        exprs <- sepBy1 (exprN $ A8) (try $ padString "%")
        let mod' a b = Var "%" :$ [a, b]
        return $ foldl1 mod' exprs
    *<|> exprN (A8)

exprN A6 =
    "append" ?: do
        exprs <- sepBy1 (exprN $ A7) (try $ padString "++")
        return $ collector "++" exprs
    *<|> exprN (A7)

exprN A5 =
    "addition/subtraction" ?: do
        -- Similar to multiply & divide
        -- eg. "1+2+3-4-5+6-7"
        --     [[1],[2],[3,4,5],[6,7]]
        exprs <- sepBy1
            (sepBy1 (exprN $ A6) (try $ padString "-" ))
            (try $ padString "+" )
        let sub a b = Var "-" :$ [a, b]
        return $ collector "+" $ map (foldl1 sub) exprs
    *<|> exprN (A6)

exprN A4 =
    do
        firstExpr <- exprN $ A5
        otherComparisonsExpr <- many $ do
            comparisonSymb <-
                     padString "=="
                *<|> padString "!="
                *<|> padString ">="
                *<|> padString "<="
                *<|> padString ">"
                *<|> padString "<"
            expr <- exprN $ A5
            return (Var comparisonSymb, expr)
        let
            (comparisons, otherExprs) = unzip otherComparisonsExpr
            exprs = firstExpr:otherExprs
        return $ case comparisons of
            []  -> firstExpr
            [x] -> x :$ exprs
            _   -> collector "all" $ zipWith3 (\c e1 e2 -> c :$ [e1,e2]) comparisons exprs (tail exprs)
    *<|> exprN (A5)

exprN A3 =
    "logical-not" ?: do
        _ <- padString "!"
        a <- exprN $ A4
        return $ Var "!" :$ [a]
    *<|> exprN (A4)

exprN A2 =
    "logical and/or" ?: do
        a <- exprN $ A3
        symb <-      padString "&&"
                *<|> padString "||"
        b <- exprN A2
        return $ Var symb :$ [a,b]
    *<|> exprN (A3)

exprN A1 =
    "ternary" ?: do
        a <- exprN $ A2
        _ <- padString "?"
        b <- exprN A1
        _ <- padString ":"
        c <- exprN A1
        return $ Var "?" :$ [a,b,c]
    *<|> exprN (A2)

exprN A0 =
    do
        _ <- genSpace
        expr <- exprN $ A1
        _ <- genSpace
        return expr
    *<|> exprN (A1)

