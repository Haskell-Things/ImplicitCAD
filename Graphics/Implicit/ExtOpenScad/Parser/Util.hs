module Graphics.Implicit.ExtOpenScad.Parser.Util where

import Text.ParserCombinators.Parsec  hiding (State)
import Graphics.Implicit.ExtOpenScad.Definitions

-- white space, including tabs, newlines and comments
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

pad parser = do
    _ <- genSpace
    a <- parser
    _ <- genSpace
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
    _ <- genSpace
    s' <- string s
    _ <- genSpace
    return s'

tryMany = (foldl1 (<|>)) . (map try)

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

