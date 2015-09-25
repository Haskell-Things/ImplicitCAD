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
expr0 = exprN 0

exprN :: Integer -> GenParser Char st Expr

exprN 12 =
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

exprN n@11 =
    do
        obj <- exprN $ n+1
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
    *<|> (exprN $ n+1 )

exprN n@10 = 
    "negation" ?: do
        _ <- padString "-"
        expr <- exprN $ n+1
        return $ Var "negate" :$ [expr]
    *<|> do
        _ <- padString "+"
        expr <- exprN $ n+1
        return expr
    *<|> exprN (n+1)

exprN n@9 = 
    "exponentiation" ?: do 
        a <- exprN $ n+1
        _ <- padString "^"
        b <- exprN n
        return $ Var "^" :$ [a,b]
    *<|> exprN (n+1)

exprN n@8 = 
    "multiplication/division" ?: do 
        -- outer list is multiplication, inner division.
        -- eg. "1*2*3/4/5*6*7/8"
        --     [[1],[2],[3,4,5],[6],[7,8]]
        exprs <- sepBy1 
            (sepBy1 (exprN $ n+1) (try $ padString "/" )) 
            (try $ padString "*" )
        let div  a b = Var "/" :$ [a, b]
        return $ collector "*" $ map (foldl1 div) exprs
    *<|> exprN (n+1)

exprN n@7 =
    "modulo" ?: do 
        exprs <- sepBy1 (exprN $ n+1) (try $ padString "%")
        let mod  a b = Var "%" :$ [a, b]
        return $ foldl1 mod exprs 
    *<|> exprN (n+1)

exprN n@6 =
    "append" ?: do 
        exprs <- sepBy1 (exprN $ n+1) (try $ padString "++")
        return $ collector "++" exprs
    *<|> exprN (n+1)

exprN n@5 =
    "addition/subtraction" ?: do 
        -- Similar to multiply & divide
        -- eg. "1+2+3-4-5+6-7" 
        --     [[1],[2],[3,4,5],[6,7]]
        exprs <- sepBy1 
            (sepBy1 (exprN $ n+1) (try $ padString "-" )) 
            (try $ padString "+" )
        let sub a b = Var "-" :$ [a, b]
        return $ collector "+" $ map (foldl1 sub) exprs
    *<|> exprN (n+1)

exprN n@4 = 
    do
        firstExpr <- exprN $ n+1
        otherComparisonsExpr <- many $ do
            comparisonSymb <-
                     padString "=="
                *<|> padString "!="
                *<|> padString ">="
                *<|> padString "<="
                *<|> padString ">"
                *<|> padString "<"
            expr <- exprN $ n+1
            return (Var comparisonSymb, expr) 
        let
            (comparisons, otherExprs) = unzip otherComparisonsExpr
            exprs = firstExpr:otherExprs
        return $ case comparisons of 
            []  -> firstExpr
            [x] -> x :$ exprs
            _   -> collector "all" $ zipWith3 (\c e1 e2 -> c :$ [e1,e2]) comparisons exprs (tail exprs)
    *<|> exprN (n+1)

exprN n@3 =
    "logical-not" ?: do
        _ <- padString "!"
        a <- exprN $ n+1
        return $ Var "!" :$ [a]
    *<|> exprN (n+1)

exprN n@2 = 
    "logical and/or" ?: do 
        a <- exprN $ n+1
        symb <-      padString "&&"
                *<|> padString "||"
        b <- exprN n
        return $ Var symb :$ [a,b]
    *<|> exprN (n+1)

exprN n@1 = 
    "ternary" ?: do 
        a <- exprN $ n+1
        _ <- padString "?"
        b <- exprN n
        _ <- padString ":"
        c <- exprN n
        return $ Var "?" :$ [a,b,c]
    *<|> exprN (n+1)

exprN n@0 = 
    do 
        _ <- genSpace
        expr <- exprN $ n+1
        _ <- genSpace
        return expr
    *<|> exprN (n+1)

