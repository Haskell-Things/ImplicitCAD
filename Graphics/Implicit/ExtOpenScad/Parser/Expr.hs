module Graphics.Implicit.ExtOpenScad.Parser.Expr where

import Graphics.Implicit.Definitions
import Text.ParserCombinators.Parsec  hiding (State)
import Text.ParserCombinators.Parsec.Expr
import Graphics.Implicit.ExtOpenScad.Definitions
import Graphics.Implicit.ExtOpenScad.Parser.Util

infixr 1 *<|>
a *<|> b = try a <|> b

infixr 2 ?:
l ?: p = p <?> l

variable :: GenParser Char st Expr
variable = fmap Var variableSymb

literal :: GenParser Char st Expr
literal = ("literal" ?:) $
	"boolean" ?: do
		b  <-    (string "true" >> return True)
		    *<|> (string "false" >> return False)
		return $ LitE $ OBool b
	*<|> "number" ?: (
		do
			a <- many1 digit
			char '.'
			b <- many digit
			return $ LitE $ ONum (read (a ++ "." ++ b) :: ℝ)
		*<|>  do
			a <- many1 digit
			return $ LitE $ ONum (read a :: ℝ)
		)
	*<|> "string" ?: do
		string "\""
		strlit <-  many $ (string "\\\"" >> return '\"') 
		             *<|> (string "\\n" >> return '\n')
		             *<|> ( noneOf "\"\n")
		string "\""
		return $ LitE $ OString strlit

-- We represent the priority or 'fixity' of different types of expressions
-- by the Int argument

expression :: Int -> GenParser Char st Expr

expression n@12 = 
	     literal
	*<|> variable
	*<|> "bracketed expression" ?: do
		-- eg. ( 1 + 5 )
		string "("
		expr <- expression 0
		string ")"
		return expr
	*<|> "vector/list" ?: (
		do
			-- eg. [ 3, a, a+1, b, a*b ]
			string "["
			exprs <- sepBy (expression 0) (char ',' )
			string "]"
			return $ ListE exprs
		*<|> do 
			-- eg. ( 1,2,3 )
			string "("
			exprs <- sepBy (expression 0) (char ',' )
			string ")"
			return $ ListE exprs
		)
	*<|> "vector/list generator" ?: do
		-- eg.  [ a : 1 : a + 10 ]
		string "["
		exprs <- sepBy (expression 0) (char ':' )
		string "]"
		return $ collector "list_gen" exprs

expression n@11 = 
	let
		posMatch a =
			do
				x <- a
				return $ Just x
			*<|> (return Nothing)
		modifier = 
			"function application" ?: do
				genSpace
				string "("
				genSpace
				args <- sepBy 
					(expression 0) 
					(try $ genSpace >> char ',' >> genSpace)
				genSpace
				string ")"
				genSpace
				return $ \f -> f :$ args
			*<|> "list indexing" ?: do
				genSpace
				string "["
				i <- pad $ expression 0
				string "]"
				genSpace
				return $ \l -> Var "index" :$ [l, i]
			*<|> "list splicing" ?: do
				string "["
				genSpace
				start <- posMatch $ expression 0
				genSpace
				char ':'
				genSpace
				end   <- posMatch $ expression 0
				genSpace
				string "]"
				return $ case (start, end) of
					(Nothing, Nothing) -> id
					(Just s,  Nothing)  -> \l -> Var "splice" :$ [l, s, LitE OUndefined ]
					(Nothing, Just e )  -> \l -> Var "splice" :$ [l, LitE $ ONum 0, e]
					(Just s,  Just e )  -> \l -> Var "splice" :$ [l, s, e]
		
	in 
		"list splicing" ?: do
			obj <- expression $ n+1
			genSpace
			mods <- modifier `sepBy` (genSpace)
			genSpace
			return $ foldl (\a b -> b a) obj mods
		*<|> (expression $ n+1 )

expression n@10 = 
	do
		char '-'
		genSpace
		expr <- expression $ n+1
		return $ Var "negate" :$ [expr]
	*<|> do
		char '+'
		genSpace
		expr <- expression $ n+1
		return expr
	*<|> (expression $ n+1)

expression n@9 = 
	"exponentiation" ?: do 
		a <- expression (n+1)
		genSpace
		string "^"
		genSpace
		b <- expression n;
		return $ Var "^" :$ [a,b]
	*<|> (expression $ n+1)

expression n@8 = 
	"multiplication/division" ?: do 
		-- outer list is multiplication, inner division. objects are 
		-- expressions and take a varlookup to evaluate.
		-- eg. "1*2*3/4/5*6*7/8"
		--     [[vl→1],[vl→2],[vl→3,vl→4,vl→5],[vl→6],[vl→7,vl→8]]
		exprs <- sepBy1 (sepBy1 (pad $ expression $ n+1) 
			(try $ genSpace >> char '/' >> genSpace )) 
			(try $ genSpace >> char '*' >> genSpace)
		--     [[1],[2],[3,4,5],[6],[7,8]]
		--     [ 1,  2,  3/4/5,  6,  7/8 ]
		--       1 * 2 * 3/4/5 * 6 * 7/8 
		let div  a b = Var "/" :$ [a, b]
		return $ collector "*" $ map (foldl1 div) exprs
	*<|> (expression $ n+1)

expression n@7 =
	"modulo" ?: do 
		let mod  a b = Var "%" :$ [a, b]
		exprs <- sepBy1 (expression $ n+1) (try $ genSpace >> string "%" >> genSpace)
		return $ foldl1 mod exprs 
	*<|> (expression $ n+1)

expression n@6 =
	"append" ?: do 
		exprs <- sepBy1 (expression $ n+1) (try $ genSpace >> string "++" >> genSpace)
		return $ collector "++" exprs
	*<|> (expression $ n+1)

expression n@5 =
	"addition/subtraction" ?: do 
		-- Similar to multiply & divide
		-- eg. "1+2+3-4-5+6-7" 
		--     [[1],[2],[3,4,5],[6,7]]
		exprs <- sepBy1 (sepBy1 (pad $ expression $ n+1) 
			(try $ genSpace >> char '-' >> genSpace )) 
			(try $ genSpace >> char '+' >> genSpace)
		let sub a b = Var "-" :$ [a, b]
		return $ collector "+" $ map (foldl1 sub) exprs
	*<|> (expression $ n+1)

expression n@4 = 
	do
		firstExpr <- expression $ n+1
		otherComparisonsExpr <- many $ do
			comparisonSymb <-
				     string "=="
				*<|> string "!="
				*<|> string ">="
				*<|> string "<="
				*<|> string ">"
				*<|> string "<"
			expr <- expression $ n+1
			return (Var comparisonSymb, expr) 
		let
			(comparisons, otherExprs) = unzip otherComparisonsExpr
			exprs = firstExpr:otherExprs
		return $ case comparisons of 
			[]  -> firstExpr
			[x] -> x :$ exprs
			_   -> collector "all" $ zipWith3 (\c e1 e2 -> c :$ [e1,e2]) comparisons exprs (tail exprs)
	*<|> (expression $ n+1)

expression n@3 =
	"logical-not" ?: do
		string "!"
		genSpace
		a <- expression $ n+1
		return $ Var "!" :$ [a]
	*<|> (expression $ n+1)

expression n@2 = 
	"logical and/or" ?: do 
		a <- expression (n+1)
		genSpace
		symb <-    string "&&"
		      *<|> string "||"
		genSpace
		b <- expression n
		return $ Var symb :$ [a,b]
	*<|> (expression $ n+1)

expression n@1 = 
	"ternary" ?: do 
		a <- expression (n+1)
		genSpace
		string "?"
		genSpace
		b <- expression n
		genSpace
		string ":"
		genSpace
		c <- expression n
		return $ Var "?" :$ [a,b,c]
	*<|> (expression $ n+1)

expression n@0 = 
	do 
		genSpace
		expr <- expression $ n+1
		genSpace
		return expr
	*<|> (expression $ n+1)

