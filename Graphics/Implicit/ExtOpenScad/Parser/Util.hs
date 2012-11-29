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



