module Graphics.Implicit.ExtOpenScad.Parser.Util where

import Graphics.Implicit.Definitions
import Text.ParserCombinators.Parsec  hiding (State)
import Text.ParserCombinators.Parsec.Expr
import Graphics.Implicit.ExtOpenScad.Definitions

-- white space, including tabs, newlines and comments
genSpace = many $ 
    oneOf " \t\n\r" 
    <|> (try $ do
        string "//"
        many ( noneOf "\n")
        string "\n"
        return ' '
    ) <|> (try $ do
        string "/*"
        manyTill anyChar (try $ string "*/")
        return ' '
    )

pad parser = do
    genSpace
    a <- parser
    genSpace
    return a

infixr 1 *<|>
a *<|> b = try a <|> b

infixr 2 ?:
l ?: p = p <?> l

stringGS (' ':xs) = do
    x'  <- genSpace
    xs' <- stringGS xs
    return (x' ++ xs')
stringGS (x:xs) = do
    x'  <- char x
    xs' <- stringGS xs
    return (x' : xs')
stringGS "" = return ""

padString s = do
    genSpace
    s' <- string s
    genSpace
    return s'

tryMany = (foldl1 (<|>)) . (map try)

variableSymb = many1 (noneOf " ,|[]{}()+-*&^%#@!~`'\"\\/;:.,<>?=") <?> "variable"


patternMatcher :: GenParser Char st Pattern
patternMatcher =
    (do 
        char '_'
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
        char '['
        genSpace
        components <- patternMatcher `sepBy` (try $ genSpace >> char ',' >> genSpace)
        genSpace
        char ']'
        return $ ListP components
    )



