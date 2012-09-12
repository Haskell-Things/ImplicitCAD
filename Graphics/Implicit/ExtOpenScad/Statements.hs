-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

-- We'd like to parse openscad code, with some improvements, for backwards compatability.

-- Implement statements for things other than primitive objects!

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts, TypeSynonymInstances, UndecidableInstances, ScopedTypeVariables, NoMonomorphismRestriction  #-}

module Graphics.Implicit.ExtOpenScad.Statements where

import Prelude hiding (lookup)
import Graphics.Implicit.Definitions
import Graphics.Implicit.ObjectUtil (getBox2, getBox3)
import Graphics.Implicit.ExtOpenScad.Definitions
import Graphics.Implicit.ExtOpenScad.Expressions
import Graphics.Implicit.ExtOpenScad.Util
import Graphics.Implicit.ExtOpenScad.Primitives
import qualified Graphics.Implicit.Primitives as Prim
import Data.Map (Map, lookup, insert, union)
import Text.ParserCombinators.Parsec 
import Text.ParserCombinators.Parsec.Expr
import Control.Monad (liftM)
import System.Plugins.Load (load_, LoadStatus(..))
import Control.Monad (forM_)
import Graphics.Implicit.ExtOpenScad.Util.ArgParser
import Graphics.Implicit.ExtOpenScad.Util.Computation

tryMany = (foldl1 (<|>)) . (map try)


-- | A statement in our programming openscad-like programming language.
computationStatement :: GenParser Char st ComputationStateModifier
computationStatement = 
	(try $ do -- suite statemetns: no semicolon...
		genSpace
		s <- tryMany [
			ifStatement,
			forStatement,
			throwAway,
			userModuleDeclaration,
			unimplemented "mirror",
			unimplemented "multmatrix",
			unimplemented "color",
			unimplemented "render",
			unimplemented "surface",
			unimplemented "projection",
			unimplemented "rotate_extrude",
			unimplemented "import_stl"
			-- rotateExtrudeStatement
			]
		genSpace
		return s
	) <|> (try $ do -- Non suite statements. Semicolon needed...
		genSpace
		s <- tryMany [
			echoStatement,
			assigmentStatement,
			includeStatement,
			useStatement
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



-- | A suite of statements!
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
--  We consider it to be a list of statements which
--  are in tern ComputationStateModifier s.
--  So this parses them.

suite :: GenParser Char st [ComputationStateModifier]
suite = (liftM return computationStatement <|> do 
	char '{'
	genSpace
	stmts <- many (try computationStatement)
	genSpace
	char '}'
	return stmts
	) <?> "statement suite"

throwAway :: GenParser Char st ComputationStateModifier
throwAway = do
	genSpace
	oneOf "%*"
	genSpace
	computationStatement
	return id

-- An included statement! Basically, inject another openscad file here...
includeStatement :: GenParser Char st ComputationStateModifier
includeStatement = (do
	line <- fmap sourceLine getPosition
	string "include"
	genSpace
	string "<"
	filename <- many (noneOf "<>")
	string ">"
	return $ \ ioWrappedState -> do
		state@(varlookup,obj2s,obj3s) <- ioWrappedState;
		case reverse filename of
			'o':'.':_ -> do
				loaded :: LoadStatus VariableLookup
					<- load_ filename ["."] "openscadAPI"
				case loaded of
					LoadFailure errs -> do
						putStrLn $ show errs
						return state
					LoadSuccess _ newapi -> do
						putStrLn "Loaded Haskell Module..."
						return (union varlookup newapi, obj2s, obj3s)
			_ -> do
				content <- readFile filename
				case parse (many1 computationStatement) ""  content of
					Left  err ->  do
						errorMessage line $ 
							"Error parsing included file <file>" ++ filename ++ "</file>\n"
							++ show err
							++ "Ignoring included file <file>" ++ filename ++ "</file>..."
						return state
					Right result -> runComputations (return state) result
	) <?> "include statement"

-- In a use statement, variables are imported but we drop any existing 2D/3D objects.
useStatement :: GenParser Char st ComputationStateModifier
useStatement = (do
	line <- fmap sourceLine getPosition
	string "use"
	genSpace
	string "<"
	filename <- many (noneOf "<>")
	string ">"
	return $ \ ioWrappedState -> do
		state@(varlookup, _, _) <- ioWrappedState;
		content <- readFile filename
		case parse (many1 computationStatement) ""  content of
			Left  err ->  do
				errorMessage line $ 
					"Error parsing included file <file>" ++ filename ++ "</file>\n"
					++ show err
					++ "Ignoring included file <file>" ++ filename ++ "</file>..."
				return state
			Right result -> runComputations (return (varlookup,[],[])) result
	) <?> "use statement"


-- | An assignment statement (parser)
assigmentStatement :: GenParser Char st ComputationStateModifier
assigmentStatement = 
	(try $ do
		line <- fmap sourceLine getPosition
		pattern <- patternMatcher
		genSpace
		char '='
		genSpace
		valExpr <- expression 0
		return $ \ ioWrappedState -> do
			state@(varlookup, obj2s, obj3s) <- ioWrappedState
			let
				val = valExpr varlookup
				match = pattern val
			case match of
				Just dictWithNew -> case val of
					OError e -> do
						errorMessage line $ 
							"error in evaluating assignment statement assigned value:"
							++ concat (map ("\n   "++) e)
						return (union dictWithNew varlookup, obj2s, obj3s) 
					_ -> return (union dictWithNew varlookup, obj2s, obj3s) 
				Nothing -> do
					errorMessage line $ "pattern match fail in assignment statement"
					return state
	) <|> (try $ do 
		line <- fmap sourceLine getPosition
		varSymb <- (try $ string "function" >> space >> genSpace >> variableSymb) 
		            <|> variableSymb
		genSpace
		char '('
		genSpace
		argVars <- sepBy variableSymb (try $ genSpace >> char ',' >> genSpace)
		genSpace
		char ')'
		genSpace
		char '='
		genSpace
		valExpr <- expression 0
		return $ \ ioWrappedState -> do
			(varlookup, obj2s, obj3s) <- ioWrappedState
			let
				makeFunc baseExpr (argVar:xs) varlookup' = OFunc $ 
					\argObj -> makeFunc baseExpr xs (insert argVar argObj varlookup')
				makeFunc baseExpr [] varlookup' = baseExpr varlookup'
				val = makeFunc valExpr argVars varlookup
			case val of
				OError e -> do
					errorMessage line $ "error in evaluating assignment statement assigned value:"
						++ concat (map ("\n   "++) e)
					return (insert varSymb val varlookup, obj2s, obj3s)
				_ -> return (insert varSymb val varlookup, obj2s, obj3s)
	)<?> "assignment statement"

-- | An echo statement (parser)
echoStatement :: GenParser Char st ComputationStateModifier
echoStatement = do
	line <- fmap sourceLine getPosition
	string "echo"
	genSpace
	char '('
	genSpace
	exprs <- expression 0 `sepBy` (try $ genSpace >> char ',' >> genSpace)
	genSpace
	char ')'
	return $  \ ioWrappedState -> do
		state@(varlookup, _, _) <- ioWrappedState
		let 
			vals = map ($varlookup) exprs
			isError (OError _) = True
			isError _ = False
			show2 (OString str) = str
			show2 a = show a 
		if any isError vals 
			then errorMessage line $
				"in module <module>echo</module>:"
				++ ( concat $ concat $ 
					map (map ("\n   "++)) $ 
						map (\(OError errs) -> errs) $ filter isError vals
				   )
			else putStrLn $
				unwords $ map show2 vals

		return state

ifStatement :: GenParser Char st ComputationStateModifier
ifStatement = (do
	line <- fmap sourceLine getPosition
	string "if"
	genSpace
	char '('
	bexpr <- expression 0
	char ')'
	genSpace
	statementsTrueCase <- suite
	genSpace
	statementsFalseCase <- try (string "else" >> genSpace >> suite ) <|> (return [])
	return $  \ ioWrappedState -> do
		state@(varlookup, _, _) <- ioWrappedState
		case bexpr varlookup of
			OBool bval -> 
				if bval
				then runComputations (return state) statementsTrueCase
				else runComputations (return state) statementsFalseCase
			OError errs -> do
				errorMessage line $ " error while evaluating if statement conditional:" 
				         ++ concat (map ("\n    " ++) errs)
				return state
			obj -> do
				errorMessage line $ "inappropriate type for if statement conditional:\n"
				        ++ "   value " ++ show obj ++ " is not a boolean."
				return state
	) <?> "if statement"

forStatement :: GenParser Char st ComputationStateModifier
forStatement = (do
	line <- fmap sourceLine getPosition
	-- a for loop is of the form:
	--      for ( vsymb = vexpr   ) loopStatements
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
	loopStatements <- suite
	return $ \ ioWrappedState -> do
		-- a for loop unpackages the state from an io monad
		state@(varlookup,_,_) <- ioWrappedState;
		let
			-- each iteration of the loop consists of unpacking the state
			loopOnce :: 
				ComputationState    -- ^ The state at this point in the loop
				-> OpenscadObj      -- ^ The value of vsymb for this iteration
				-> ComputationState -- ^ The resulting state
			loopOnce ioWrappedState val =  do
				state@(varlookup, a, b) <- ioWrappedState;
				let
					match = pattern val
					vsymbSetState = case match of
						Just dictWithNew -> return (union dictWithNew varlookup, a, b) 
						Nothing -> do
							errorMessage line $ "Pattern match fail in for loop step"
							return state
				runComputations vsymbSetState loopStatements
		-- Then loops once for every entry in vexpr
		case vexpr varlookup of 
			OList l -> foldl (loopOnce) (return state) l
			OError errs -> do
				errorMessage line $ "Error while evaluating for loop array:" 
				         ++ concat (map ("\n    " ++) errs)
				return state
			obj     -> do
				errorMessage line $ "Error in for loop iteration array:\n"
				        ++ "   Inappropriate type for loop iterated array:\n"
				        ++ "       value " ++ show obj ++ " is not a list."
				return state
	) <?> "for statement"

moduleWithSuite ::
	String -> ([ComputationStateModifier] -> ArgParser ComputationStateModifier)
	-> GenParser Char st ComputationStateModifier
moduleWithSuite name argHandeler = (do
	line <- fmap sourceLine getPosition
	string name;
	genSpace;
	(unnamed, named) <- moduleArgsUnit
	genSpace;
	statements <- suite
	return $ \ ioWrappedState -> do
		state@(varlookup, obj2s, obj3s) <- ioWrappedState
		case argMap 
			(map ($varlookup) unnamed) 
			(map (\(a,b) -> (a, b varlookup)) named) (argHandeler statements)
			of
				(Just computationModifier, []) ->  computationModifier (return state)
				(Nothing, []) -> do
					errorMessage line $ "Module <module>" ++ name 
						++ "</module> failed without a message"
					return state
				(Nothing, errs) -> do
					errorMessage line $  "Module <module>" ++ name 
						++ "</module> failed with the following messages:"
						++ concat (map ("  "++) errs)
					return state
				(Just computationModifier, errs) -> do
					errorMessage line $ "Module <module>" ++ name 
						++ "</module> gave the following warnings:"
						++ concat (map ("  "++) errs)
					computationModifier (return state)
	) <?> (name ++ " statement")

unimplemented :: String -> GenParser Char st ComputationStateModifier
unimplemented name = do
	line <- fmap sourceLine getPosition
	string name
	genSpace;
	moduleArgsUnit
	genSpace;
	(try suite <|> (genSpace >> char ';' >> return []))
	return $ \ ioWrappedState -> do
		state <- ioWrappedState
		errorMessage line $ "OpenSCAD command " ++ name ++ " not yet implemented"
		return state


userModule :: GenParser Char st ComputationStateModifier
userModule = do
	line <- fmap sourceLine getPosition
	name <- variableSymb;
	genSpace;
	(unnamed, named) <- moduleArgsUnit
	genSpace;
	statements <- ( try suite <|> (genSpace >> char ';' >> return []))
	return $ \ ioWrappedState -> do
		state@(varlookup, obj2s, obj3s) <- ioWrappedState
		case lookup name varlookup of
			Just (OModule m) -> 
				case argMap 
					(map ($varlookup) unnamed) 
					(map (\(a,b) -> (a, b varlookup)) named) 
					(m statements)
				of
				(Just computationModifier, []) ->  
					computationModifier (return state)
				(Nothing, []) -> do
					errorMessage line $ "Module <module>" ++ name 
						++ "</module> failed without a message"
					return state
				(Nothing, errs) -> do
					errorMessage line $  "Module <module>" ++ name 
						++ "</module> failed with the following messages:"
						++ concat (map ("  "++) errs)
					return state
				(Just computationModifier, errs) -> do
					errorMessage line $ "Module <module>" ++ name 
						++ "</module> gave the following warnings:"
						++ concat (map ("  "++) errs)
					computationModifier (return state)
			_ -> do
				errorMessage line $  "module <module>" ++ name ++ "</module> is not in scope"
				return state


userModuleDeclaration :: GenParser Char st ComputationStateModifier
userModuleDeclaration = do
	string "module"
	genSpace;
	newModuleName <- variableSymb;
	genSpace;
	args <- moduleArgsUnitDecl
	genSpace;
	codeStatements <- suite
	return $ \ envIOWrappedState -> do
		(envVarlookup, envObj2s, envObj3s) <- envIOWrappedState
		let 
			newModule = OModule $ \childrenStatements -> do 
				argVarlookupModifier <- args envVarlookup
				return $ \contextIOWrappedState -> do
					contextState@(contextVarLookup, contextObj2s, contextObj3s)
						<- contextIOWrappedState
					(_, childObj2s, childObj3s) <- runComputations 
						(return contextState)
						childrenStatements;
					let
						children = ONum $ fromIntegral  
							(length childObj2s + length childObj3s)
						child = OModule $ \suite -> do
							n :: ℕ <- argument "n";
							if n <= length childObj3s 
							         then addObj3 (childObj3s !! n)
							         else addObj2 (childObj2s !! (n+1-length childObj3s))
						childBox = OFunc $ \n -> case fromOObj n :: Maybe ℕ of
							Just n  | n < length childObj3s + length childObj2s ->  
								if n <= length childObj3s 
							         then toOObj $ getBox3 (childObj3s !! n)
							         else toOObj $ getBox2 (childObj2s !! (n+1-length childObj3s))
							Nothing -> OUndefined
						varlookupForCode = 
							(insert "child" child) $ 
							(insert "children" children) $
							(insert "childBox" childBox) $
							(insert newModuleName newModule) $
							envVarlookup
					(_, resultObj2s, resultObj3s) 
						<- runComputations 
							(return (argVarlookupModifier varlookupForCode,[],[]))
							codeStatements
					return (
						contextVarLookup, 
						contextObj2s ++ resultObj2s, 
						contextObj3s ++ resultObj3s
						)
		return (insert newModuleName (newModule) envVarlookup, envObj2s, envObj3s)



