-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

-- We'd like to parse openscad code, with some improvements, for backwards compatability.

module Graphics.Implicit.ExtOpenScad.Expressions where

-- We need lookup from Data.Map
import Prelude hiding (lookup)
import Data.Map (Map, lookup)
import Graphics.Implicit.Definitions
import Graphics.Implicit.ExtOpenScad.Definitions
import Text.ParserCombinators.Parsec 
import Text.ParserCombinators.Parsec.Expr

pad parser = do
	many space
	a <- parser
	many space
	return a

variableSymb = many1 (noneOf " ,|[]{}()+-*&^%#@!~`'\"\\/;:.,<>?=") <?> "variable"

variable :: GenParser Char st (VariableLookup -> OpenscadObj)
variable = fmap (\varstr -> \varlookup -> case lookup varstr varlookup of
			Nothing -> OUndefined
			Just a -> a )
		variableSymb

literal :: GenParser Char st (VariableLookup -> OpenscadObj)
literal = 
	try ( (string "true" >> return (\map -> OBool True) )
		<|> (string "false" >> return (\map -> OBool False) )
		<?> "boolean" )
	<|> try ( try (do
			a <- (many1 digit);
			char '.';
			b <- (many digit);
			return ( \map -> ONum ( read (a ++ "." ++ b) :: ℝ) );
		) <|>  (do
			a <- (many1 digit);
			return ( \map -> ONum ( read a :: ℝ) );
		) <?> "number" )
	<|> try ( ( do
		string "\"";
		strlit <-  many $  try (string "\\\"" >> return '\"') <|> try (string "\\n" >> return '\n') <|> ( noneOf "\"\n");
		string "\"";
		return $ \map -> OString $ strlit;
	) <?> "string" )
	<?> "literal"

-- We represent the priority or 'fixity' of different types of expressions
-- by the Int argument

expression :: Int -> GenParser Char st (VariableLookup -> OpenscadObj)
expression 10 = (try literal) <|> (try variable )
	<|> ((do
		string "(";
		expr <- expression 0;
		string ")";
		return expr;
	) <?> "bracketed expression" )
	<|> ( try ( do
		string "[";
		exprs <- sepBy (expression 0) (char ',' );
		string "]";
		return $ \varlookup -> OList (map ($varlookup) exprs )
	) <|> ( do
		string "[";
		exprs <- sepBy (expression 0) (char ':' );
		string "]";
		return $ \varlookup -> OList $ map ONum $ case map (coerceNum.($varlookup)) exprs  of
			a:[]     -> [a]
			a:b:[]   -> [a .. b]
			a:b:c:xs -> [a, a+b .. c]
	)<?> "vector/list" )
expression 9 = 
	let
		-- Like in Haskell, we're going to think of functions of 
		-- many variables as functions that result in functions.
		-- So f(a,b) = f(a)(b) :)
		applyArgs :: OpenscadObj -> [OpenscadObj] -> OpenscadObj
		applyArgs obj []  = obj
		applyArgs (OFunc f) (arg:others) = applyArgs (f arg) others 
		applyArgs _ _ = OUndefined
		-- List splicing, like in Python. 'Cause list splicing is
		-- awesome!
		splice :: [a] -> ℝ -> ℝ -> [a]
		splice [] _ _     = []
		splice (x:xs) a b 
			| floor a < 0 =      splice xs (fromIntegral $ length xs + floor a) (fromIntegral $ floor b)
			| floor b < 0 =      splice xs (fromIntegral $ floor a) ( fromIntegral $ length xs + floor b)
			| floor a > 0 =      splice xs (fromIntegral $ floor a - 1) (fromIntegral $ floor b)
			| floor b > 0 = x : (splice xs (fromIntegral $ floor a) (fromIntegral $ floor b - 1 ) )
			| otherwise = []
	in ( try( do 
		f <- expression 10;
		many space
		string "(";
		args <- sepBy (expression 0) (many space >> char ',' >> many space);
		string ")";
		return $ \varlookup -> applyArgs (f varlookup) (map ($varlookup) args) 
	) <?> "function appliation" )
	<|> ( try( do 
		l <- expression 10;
		string "[";
		i <- expression 0;
		string "]";
		return $ \varlookup ->
			case (l varlookup, i varlookup) of
				(OList actual_list, ONum ind) -> actual_list !! (floor ind)
				(OString str, ONum ind) -> OString $ [str !! (floor ind)]
				_ -> OUndefined
	) <?> "list indexing" )
	<|> ( try( do 
		l <- expression 10;
		string "[";
		many space
		start <- (try $ expression 0) <|> (many space >> return (\_ -> OUndefined));
		many space
		char ':';
		many space
		end   <- (try $ expression 0) <|> (many space >> return (\_ -> OUndefined));
		many space
		string "]";
		return $ \varlookup ->
			case (l varlookup, start varlookup, end varlookup) of
				(OList  list, ONum a,     ONum b    ) -> OList   $ splice list a b
				(OString str, ONum a,     ONum b    ) -> OString $ splice str  a b
				(OList  list, OUndefined, ONum b    ) -> OList   $ splice list 0 b
				(OString str, OUndefined, ONum b    ) -> OString $ splice str  0 b
				(OList  list, ONum a,     OUndefined) -> OList   $ splice list a (1.0/0.0)
				(OString str, ONum a,     OUndefined) -> OString $ splice str  a (1.0/0.0)
				(OList  list, OUndefined, OUndefined) -> OList   $ splice list 0 (1.0/0.0)
				(OString str, OUndefined, OUndefined) -> OString $ splice str  0 (1.0/0.0)
				_ -> OUndefined
	) <?> "list splicing" )
	<|> try (expression 10)
expression n@8 = try (( do 
		a <- expression (n+1);
		many space
		string "^";
		many space
		b <- expression n;
		return $ \varlookup -> case (a varlookup, b varlookup) of
			(ONum na, ONum nb) -> ONum (na ** nb)
			_ -> OUndefined
	) <?> "exponentiation")
	<|> try (expression $ n+1)
expression n@7 =  try (expression $ n+1)
expression n@6 = 
	let 
		mult (ONum a)  (ONum b)  = ONum  (a*b)
		mult (ONum a)  (OList b) = OList (map (mult (ONum a)) b)
		mult (OList a) (ONum b)  = OList (map (mult (ONum b)) a)
		mult _         _         = OUndefined

		div (ONum a)  (ONum b) = ONum  (a/b)
		div (OList a) (ONum b) = OList (map (\x -> div x (ONum b)) a)
		div _         _        = OUndefined
	in try (( do 
		exprs <- sepBy1 (sepBy1 (pad $ expression $ n+1) 
			(many space >> char '/' >> many space )) 
			(many space >> char '*' >> many space)
		return $ \varlookup -> foldl1 mult $ map ( (foldl1 div) . (map ($varlookup) ) ) exprs;
	) <?> "multiplication/division")
	<|>try (expression $ n+1)
expression n@5 =
	let 
		append (OList   a) (OList   b) = OList   $ a++b
		append (OString a) (OString b) = OString $ a++b
		append _           _           = OUndefined
	in try (( do 
		exprs <- sepBy1 (expression $ n+1) (many space >> string "++" >> many space)
		return $ \varlookup -> foldl1 append $ map ($varlookup) exprs;
	) <?> "append") 
	<|>try (expression $ n+1)

expression n@4 =
	let 
		add (ONum a) (ONum b) = ONum (a+b)
		add (OList a) (OList b) = OList $ zipWith add a b
		add _ _ = OUndefined

		sub (ONum a) (ONum b) = ONum (a-b)
		sub (OList a) (OList b) = OList $ zipWith sub a b
		sub _ _ = OUndefined
	in try (( do 
		exprs <- sepBy1 (sepBy1 (pad $ expression $ n+1) 
			(many space >> char '-' >> many space )) 
			(many space >> char '+' >> many space)
		return $ \varlookup -> foldl1 add $ map ( (foldl1 sub) . (map ($varlookup) ) ) exprs;
	) <?> "addition/subtraction")
	<|>try (expression $ n+1)
expression n@3 = 
	let
		negate (ONum n) = ONum (-n)
		negate (OList l) = OList $ map negate l
		negate _ = OUndefined
	in try (do
		char '-'
		many space
		expr <- expression $ n+1
		return $ \varlookup -> negate $ expr varlookup
	) <|> try (do
		char '+'
		many space
		expr <- expression $ n+1
		return $ expr
	) <|> try (expression $ n+1)
expression n@2 = try (expression $ n+1)
expression n@1 = try (expression $ n+1)
expression n@0 = try (do { many space; expr <- expression $ n+1; many space; return expr}) <|> try (expression $ n+1)

