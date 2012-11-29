module Graphics.Implicit.ExtOpenScad.Parser.Expr where

import Graphics.Implicit.Definitions
import Text.ParserCombinators.Parsec  hiding (State)
import Text.ParserCombinators.Parsec.Expr
import Graphics.Implicit.ExtOpenScad.Definitions
import Graphics.Implicit.ExtOpenScad.Parser.Util

variable :: GenParser Char st Expr
variable = fmap Var variableSymb

literal :: GenParser Char st Expr
literal = 
	try ( (string "true" >> return (LitE $ OBool True) )
		<|> (string "false" >> return (LitE $ OBool False) )
		<?> "boolean" )
	<|> try ( try (do
			a <- many1 digit
			char '.'
			b <- many digit
			return $ LitE $ ONum (read (a ++ "." ++ b) :: ℝ)
		) <|>  (do
			a <- many1 digit
			return $ LitE $ ONum (read a :: ℝ)
		) <?> "number" )
	<|> try ( ( do
		string "\""
		strlit <-  many $ try (string "\\\"" >> return '\"') <|> try (string "\\n" >> return '\n') <|> ( noneOf "\"\n")
		string "\""
		return $ LitE $ OString strlit
	) <?> "string" )
	<?> "literal"

-- We represent the priority or 'fixity' of different types of expressions
-- by the Int argument

expression :: Int -> GenParser Char st Expr
expression 10 = (try literal) <|> (try variable )
	<|> (try (do -- ( 1 + 5 )
		string "("
		expr <- expression 0
		string ")"
		return expr
	) <?> "bracketed expression" )
	<|> ( try ( do -- [ 3, a, a+1, b, a*b ]
		string "["
		exprs <- sepBy (expression 0) (char ',' )
		string "]"
		return $ ListE exprs
	)<|> try ( do -- ( 1,2,3 )
		string "("
		exprs <- sepBy (expression 0) (char ',' )
		string ")"
		return $ ListE exprs
	) <|> ( do -- eg.  [ a : 1 : a + 10 ]
		string "["
		exprs <- sepBy (expression 0) (char ':' )
		string "]"
		return $ collector "list_gen" exprs
	)<?> "vector/list" )
expression 9 = 
	let
		posMatch a =
			(try $ do
				x <- a
				return $ Just x
			) <|> (return Nothing)
		modifier = 
			(try $ (do
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
			<?> "function application"
			)) <|> (try $ (do
				genSpace
				string "["
				i <- pad $ expression 0
				string "]"
				genSpace
				return $ \l -> Var "index" :$ [l, i]
			<?> "list indexing"
			)) <|> (try $ ( do
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
			<?> "list splicing"))
		
	in ( try( do 
		obj <- expression 10
		genSpace
		mods <- modifier `sepBy` (genSpace)
		genSpace
		return $ foldl (\a b -> b a) obj mods
		) <?> "list splicing" )
	<|> try (expression 10)
expression n@8 = try (( do 
		a <- expression (n+1)
		genSpace
		string "^"
		genSpace
		b <- expression n;
		return $ Var "^" :$ [a,b]
	) <?> "exponentiation")
	<|> try (expression $ n+1)
expression n@7 = 
	let 
		div  a b = Var "/" :$ [a, b]
	in try (( do 
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
		return $ collector "*" $ map (foldl1 div) exprs
	) <?> "multiplication/division")
	<|>try (expression $ n+1)
expression n@6 =
	let 
		mod  a b = Var "%" :$ [a, b]
	in try (( do 
		exprs <- sepBy1 (expression $ n+1) (try $ genSpace >> string "%" >> genSpace)
		return $ foldl1 mod exprs
	) <?> "modulo") 
	<|>try (expression $ n+1)
expression n@5 =
	try (( do 
		exprs <- sepBy1 (expression $ n+1) (try $ genSpace >> string "++" >> genSpace)
		return $ collector "++" exprs
	) <?> "append") 
	<|>try (expression $ n+1)

expression n@4 =
	let 
		sub a b = Var "-" :$ [a, b]
	in try (( do 
		-- Similar to multiply & divide
		-- eg. "1+2+3-4-5+6-7" 
		--     [[1],[2],[3,4,5],[6,7]]
		exprs <- sepBy1 (sepBy1 (pad $ expression $ n+1) 
			(try $ genSpace >> char '-' >> genSpace )) 
			(try $ genSpace >> char '+' >> genSpace)
		return $ collector "+" $ map (foldl1 sub) exprs
	) <?> "addition/subtraction")
	<|>try (expression $ n+1)
expression n@3 = 
	let
		negate x = Var "negate" :$ [x]
	in try (do
		char '-'
		genSpace
		expr <- expression $ n+1
		return $ negate expr
	) <|> try (do
		char '+'
		genSpace
		expr <- expression $ n+1
		return expr
	) <|> try (expression $ n+1)
expression n@2 = try (expression $ n+1)
expression n@1 = 
	try ( do
		firstExpr <- expression $ n+1
		otherComparisonsExpr <- many $ do
			comparison <-
				    (try $ string "==" >> return (Var "==") )
				<|> (try $ string "!=" >> return (Var "!=") )
				<|> (try $ string ">=" >> return (Var ">=") )
				<|> (try $ string "<=" >> return (Var "<=") )
				<|> (try $ string ">"  >> return (Var ">")  )
				<|> (try $ string "<"  >> return (Var "<")  )
			expr <- expression $ n+1
			return (comparison, expr) 
		let
			(comparisons, otherExprs) = unzip otherComparisonsExpr
			exprs = firstExpr:otherExprs
		return $ case comparisons of 
			[]  -> firstExpr
			[x] -> x :$ exprs
			_   -> collector "all" [(comparisons!!n) :$ [exprs!!n, exprs!!(n+1)] | n <- [0.. length comparisons - 1] ]
	)<|> try (expression $ n+1)
expression n@0 = try (do { genSpace; expr <- expression $ n+1; genSpace; return expr}) <|> try (expression $ n+1)

