module Graphics.Implicit.ExtOpenScad.Parser.Statement where

import Graphics.Implicit.Definitions
import Text.ParserCombinators.Parsec  hiding (State)
import Text.ParserCombinators.Parsec.Expr
import Graphics.Implicit.ExtOpenScad.Definitions
import Graphics.Implicit.ExtOpenScad.Parser.Util
import Graphics.Implicit.ExtOpenScad.Parser.Expr

-- | A  in our programming openscad-like programming language.
computation :: GenParser Char st StatementI
computation = 
	(try $ do -- suite statemetns: no semicolon...
		genSpace
		s <- tryMany [
			ifStatementI,
			forStatementI,
			throwAway,
			userModuleDeclaration{-,
			unimplemented "mirror",
			unimplemented "multmatrix",
			unimplemented "color",
			unimplemented "render",
			unimplemented "surface",
			unimplemented "projection",
			unimplemented "import_stl"-}
			-- rotateExtrude
			]
		genSpace
		return s
	) <|> (try $ do -- Non suite s. Semicolon needed...
		genSpace
		s <- tryMany [
			echo,
			assignment,
			include--,
			--use
			]
		genSpace
		char ';'
		genSpace
		return s
	) <|> (try $ do
		genSpace
		s <- userModule
		genSpace
		return s
	)

{-
-- | A suite of s!
--   What's a suite? Consider:
--
--      union() {
--         sphere(3);
--      }
--
--  The suite was in the braces ({}). Similarily, the
--  following has the same suite:
--
--      union() sphere(3);
--
--  We consider it to be a list of s which
--  are in tern StatementI s.
--  So this parses them.
-}
suite :: GenParser Char st [StatementI]
suite = (fmap return computation <|> do 
	char '{'
	genSpace
	stmts <- many (try computation)
	genSpace
	char '}'
	return stmts
	) <?> " suite"


throwAway :: GenParser Char st StatementI
throwAway = do
	line <- lineNumber
	genSpace
	oneOf "%*"
	genSpace
	computation
	return $ StatementI line DoNothing

-- An included ! Basically, inject another openscad file here...
include :: GenParser Char st StatementI
include = (do
	line <- lineNumber
	use <-  (string "include" >> return False)
	    <|> (string "use"     >> return True )
	genSpace
	string "<"
	filename <- many (noneOf "<>")
	string ">"
	return $ StatementI line $ Include filename use
	) <?> "include "

-- | An assignment  (parser)
assignment :: GenParser Char st StatementI
assignment = 
	(try $ do
		line <- lineNumber
		pattern <- patternMatcher
		genSpace
		char '='
		genSpace
		valExpr <- expression 0
		return $ StatementI line$ pattern := valExpr
	) <|> (try $ do 
		line <- lineNumber
		varSymb <- (try $ string "function" >> space >> genSpace >> variableSymb) 
		            <|> variableSymb
		genSpace
		char '('
		genSpace
		argVars <- sepBy patternMatcher (try $ genSpace >> char ',' >> genSpace)
		genSpace
		char ')'
		genSpace
		char '='
		genSpace
		valExpr <- expression 0
		return $ StatementI line $ Name varSymb := LamE argVars valExpr
	)<?> "assignment "

-- | An echo  (parser)
echo :: GenParser Char st StatementI
echo = do
	line <- lineNumber
	string "echo"
	genSpace
	char '('
	genSpace
	exprs <- expression 0 `sepBy` (try $ genSpace >> char ',' >> genSpace)
	genSpace
	char ')'
	return $ StatementI line $ Echo exprs

ifStatementI :: GenParser Char st StatementI
ifStatementI = (do
	line <- lineNumber
	string "if"
	genSpace
	char '('
	bexpr <- expression 0
	char ')'
	genSpace
	sTrueCase <- suite
	genSpace
	sFalseCase <- try (string "else" >> genSpace >> suite ) <|> (return [])
	return $ StatementI line $ If bexpr sTrueCase sFalseCase
	) <?> "if "

forStatementI :: GenParser Char st StatementI
forStatementI = (do
	line <- lineNumber
	-- a for loop is of the form:
	--      for ( vsymb = vexpr   ) loops
	-- eg.  for ( a     = [1,2,3] ) {echo(a);   echo "lol";}
	-- eg.  for ( [a,b] = [[1,2]] ) {echo(a+b); echo "lol";}
	string "for"
	genSpace
	char '('
	genSpace
	pattern <- patternMatcher
	genSpace
	char '='
	vexpr <- expression 0
	char ')'
	genSpace
	loopContent <- suite
	return $ StatementI line $ For pattern vexpr loopContent
	) <?> "for "


userModule :: GenParser Char st StatementI
userModule = do
	line <- lineNumber
	name <- variableSymb;
	genSpace;
	args <- moduleArgsUnit
	genSpace;
	s <- ( try suite <|> (genSpace >> char ';' >> return []))
	return $ StatementI line $ ModuleCall name args s

userModuleDeclaration :: GenParser Char st StatementI
userModuleDeclaration = do
	line <- lineNumber
	string "module"
	genSpace;
	newModuleName <- variableSymb;
	genSpace;
	args <- moduleArgsUnitDecl
	genSpace;
	s <- suite
	return $ StatementI line $ NewModule newModuleName args s

----------------------

moduleArgsUnit :: GenParser Char st [(Maybe String, Expr)]
moduleArgsUnit = do
	char '(';
	genSpace
	args <- sepBy ( 
		(try $ do -- eg. a = 12
			symb <- variableSymb
			genSpace
			char '='
			genSpace
			expr <- expression 0
			return $ (Just symb, expr)
		) <|> (try $ do -- eg. a(x,y) = 12
			symb <- variableSymb;
			genSpace
			char '('
			genSpace
			argVars <- sepBy variableSymb (try $ genSpace >> char ',' >> genSpace)
			char ')'
			genSpace
			char '=';
			genSpace
			expr <- expression 0;
			return $ (Just symb, LamE (map Name argVars) expr)
		) <|> (do { -- eg. 12
			expr <- expression 0;
			return (Nothing, expr)
		})
		) (try $ genSpace >> char ',' >> genSpace)
	genSpace	
	char ')'
	return args

moduleArgsUnitDecl ::  GenParser Char st [(String, Maybe Expr)]
moduleArgsUnitDecl = do
	char '(';
	genSpace
	argTemplate <- sepBy ( 
		(try $ do
			symb <- variableSymb;
			genSpace
			char '='
			genSpace
			expr <- expression 0
			return (symb, Just expr)
		) <|> (try $ do
			symb <- variableSymb;
			genSpace
			char '('
			genSpace
			argVars <- sepBy variableSymb (try $ genSpace >> char ',' >> genSpace)
			char ')'
			genSpace
			char '='
			genSpace
			expr <- expression 0
			return (symb, Just expr)
		) <|> (do {
			symb <- variableSymb;
			return (symb, Nothing)
		})
		) (try $ genSpace >> char ',' >> genSpace);
	genSpace	
	char ')';
	return argTemplate


lineNumber = fmap sourceLine getPosition

