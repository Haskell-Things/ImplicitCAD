-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

-- We'd like to parse openscad code, with some improvements, for backwards compatability.

-- Implement statements for things other than primitive objects!

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts, TypeSynonymInstances, UndecidableInstances, ScopedTypeVariables, NoMonomorphismRestriction  #-}

module Graphics.Implicit.ExtOpenScad.Statements where

import Prelude hiding (lookup)
import Graphics.Implicit.Definitions
import Graphics.Implicit.ExtOpenScad.Definitions
import Graphics.Implicit.ExtOpenScad.Expressions
import Graphics.Implicit.ExtOpenScad.Util
import Graphics.Implicit.ExtOpenScad.Primitives
import qualified Graphics.Implicit.Primitives as Prim
import Data.Map (Map, lookup, insert, union)
import Text.ParserCombinators.Parsec 
import Text.ParserCombinators.Parsec.Expr
import Control.Monad (liftM)
import Data.Maybe (fromMaybe)
import System.Plugins.Load (load_, LoadStatus(..))
import Control.Monad (forM_)
import Graphics.Implicit.ExtOpenScad.Util.ArgParser
import Graphics.Implicit.ExtOpenScad.Util.Computation

tryMany = (foldl1 (<|>)) . (map try)

-- | A statement in our programming openscad-like programming language.
computationStatement :: GenParser Char st ComputationStateModifier
computationStatement = 
	(try $ do -- suite statemetns: no semicolon...
		many space
		s <- tryMany [
			ifStatement,
			forStatement, 
			unionStatement,
			intersectStatement,
			differenceStatement,
			translateStatement,
			rotateStatement,
			scaleStatement,
			extrudeStatement,
			throwAway,
			shellStatement,
			packStatement,
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
		many space
		return s
	) <|> (try $ do -- Non suite statements. Semicolon needed...
		many space
		s <- tryMany [
			echoStatement,
			assigmentStatement,
			includeStatement,
			useStatement
			]
		many space
		char ';'
		many space
		return s
	)<|> (try $ many space >> comment)
	<|> (try $ do
		many space
		s <- userModule
		many space
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
	many space
	stmts <- many (try computationStatement)
	many space
	char '}'
	return stmts
	) <?> "statement suite"


-- | We think of comments as statements that do nothing. It's just convenient.
comment = 
	(((try $ do
		string "//"
		many ( noneOf "\n")
		string "\n"
	) <|> (do
		string "/*"
		manyTill anyChar (try $ string "*/")
	)) >> return id) <?> "comment"

throwAway = do
	many space
	oneOf "%*"
	many space
	computationStatement
	return id

-- An included statement! Basically, inject another openscad file here...
includeStatement :: GenParser Char st ComputationStateModifier
includeStatement = (do
	string "include"
	many space
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
						putStrLn $ "Error parsing included file " ++ filename
						putStrLn $ show err
						putStrLn $ "Ignoring included file " ++ filename ++ "..."
						return state
					Right result -> runComputations (return state) result
	) <?> "include statement"

-- In a use statement, variables are imported but we drop any existing 2D/3D objects.
useStatement :: GenParser Char st ComputationStateModifier
useStatement = (do
	string "use"
	many space
	string "<"
	filename <- many (noneOf "<>")
	string ">"
	return $ \ ioWrappedState -> do
		state@(varlookup, _, _) <- ioWrappedState;
		content <- readFile filename
		case parse (many1 computationStatement) ""  content of
			Left  err ->  do
				putStrLn $ "Error parsing used file " ++ filename
				putStrLn $ show err
				putStrLn $ "Ignoring used file " ++ filename ++ "..."
				return state
			Right result -> runComputations (return (varlookup,[],[])) result
	) <?> "use statement"


-- | An assignment statement (parser)
assigmentStatement :: GenParser Char st ComputationStateModifier
assigmentStatement = 
	(try $ do
		pattern <- patternMatcher
		many space
		char '='
		many space
		valExpr <- expression 0
		return $ \ ioWrappedState -> do
			state@(varlookup, obj2s, obj3s) <- ioWrappedState
			let
				val = valExpr varlookup
				match = pattern val
			case match of
				Just dictWithNew -> return (union dictWithNew varlookup, obj2s, obj3s) 
				Nothing -> do
					putStrLn "Pattern match fail in assignment statement"
					return state
	) <|> (try $ do 
		varSymb <- (try $ string "function" >> many1 space >> variableSymb) 
		            <|> variableSymb
		many space
		char '('
		many space
		argVars <- sepBy variableSymb (many space >> char ',' >> many space)
		many space
		char ')'
		many space
		char '='
		many space
		valExpr <- expression 0
		return $ \ ioWrappedState -> do
			(varlookup, obj2s, obj3s) <- ioWrappedState
			let
				makeFunc baseExpr (argVar:xs) varlookup' = OFunc $ 
					\argObj -> makeFunc baseExpr xs (insert argVar argObj varlookup')
				makeFunc baseExpr [] varlookup' = baseExpr varlookup'
				val = makeFunc valExpr argVars varlookup
			return (insert varSymb val varlookup, obj2s, obj3s)
	)<?> "assignment statement"

-- | An echo statement (parser)
echoStatement :: GenParser Char st ComputationStateModifier
echoStatement = do
	string "echo"
	many space
	char '('
	many space
	exprs <- expression 0 `sepBy` (many space >> char ',' >> many space)
	many space
	char ')'
	return $  \ ioWrappedState -> do
		state@(varlookup, _, _) <- ioWrappedState
		let 
			vals = map ($varlookup) exprs
			isError (OError _) = True
			isError _ = False
			show2 (OString str) = str
			show2 a = show a
		putStrLn $ 
			if any isError vals 
			then 
				"In module echo:"
				++ ( concat $ concat $ 
					map (map ("\n   "++)) $ 
						map (\(OError errs) -> errs) $ filter isError vals
				   )
			else
				unwords $ map show2 vals

		return state

ifStatement = (do
	string "if"
	many space
	char '('
	bexpr <- expression 0
	char ')'
	many space
	statementsTrueCase <- suite
	many space
	statementsFalseCase <- try (string "else" >> many space >> suite ) <|> (return [])
	return $  \ ioWrappedState -> do
		state@(varlookup, _, _) <- ioWrappedState
		case bexpr varlookup of
			OBool bval -> 
				if bval
				then runComputations (return state) statementsTrueCase
				else runComputations (return state) statementsFalseCase
			OError errs -> do
				putStrLn ( "Error while evaluating if statement conditional:" 
				         ++ concat (map ("\n    " ++) errs)
				         )
				return state
			obj -> do
				putStrLn $ "Inappropriate type for if statement conditional:\n"
				        ++ "   value " ++ show obj ++ " is not a boolean."
				return state
	) <?> "if statement"

forStatement = (do
	-- a for loop is of the form:
	--      for ( vsymb = vexpr   ) loopStatements
	-- eg.  for ( a     = [1,2,3] ) {echo(a);   echo "lol";}
	-- eg.  for ( [a,b] = [[1,2]] ) {echo(a+b); echo "lol";}
	string "for"
	many space
	char '('
	many space
	pattern <- patternMatcher
	many space
	char '='
	vexpr <- expression 0
	char ')'
	many space
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
							putStrLn "Pattern match fail in for loop step"
							return state
				runComputations vsymbSetState loopStatements
		-- Then loops once for every entry in vexpr
		case vexpr varlookup of 
			OList l -> foldl (loopOnce) (return state) l
			OError errs -> do
				putStrLn ( "Error while evaluating for loop array:" 
				         ++ concat (map ("\n    " ++) errs)
				         )
				return state
			obj     -> do
				putStrLn $ "Error in for loop iteration array:\n"
				        ++ "   Inappropriate type for loop iterated array:\n"
				        ++ "       value " ++ show obj ++ " is not a list."
				return state
	) <?> "for statement"

moduleWithSuite ::
	String -> ([ComputationStateModifier] -> ArgParser ComputationStateModifier)
	-> GenParser Char st ComputationStateModifier
moduleWithSuite name argHandeler = (do
	string name;
	many space;
	(unnamed, named) <- moduleArgsUnit
	many space;
	statements <- suite
	return $ \ ioWrappedState -> do
		state@(varlookup, obj2s, obj3s) <- ioWrappedState
		case argMap 
			(map ($varlookup) unnamed) 
			(map (\(a,b) -> (a, b varlookup)) named) (argHandeler statements)
			of
				(Just computationModifier, []) ->  computationModifier (return state)
				(Nothing, []) -> do
					putStrLn $ "Module " ++ name ++ " failed without a message"
					return state
				(Nothing, errs) -> do
					putStrLn $ "Module " ++ name ++ " failed with the following messages:"
					forM_ errs (\err -> putStrLn $ "  " ++ err)
					return state
				(Just computationModifier, errs) -> do
					putStrLn $ "Module " ++ name ++ " gave the following warnings:"
					forM_ errs (\err -> putStrLn $ "  " ++ err)
					computationModifier (return state)
	) <?> (name ++ " statement")

unimplemented :: String -> GenParser Char st ComputationStateModifier
unimplemented name = do
	string name
	many space;
	moduleArgsUnit
	many space;
	(try suite <|> (many space >> char ';' >> return []))
	return $ \ ioWrappedState -> do
		state <- ioWrappedState
		putStrLn $ "OpenSCAD command " ++ name ++ " not yet implemented"
		return state


userModule :: GenParser Char st ComputationStateModifier
userModule = do
	name <- variableSymb;
	many space;
	(unnamed, named) <- moduleArgsUnit
	many space;
	statements <- ( try suite <|> (many space >> char ';' >> return []))
	return $ \ ioWrappedState -> do
		state@(varlookup, obj2s, obj3s) <- ioWrappedState
		case lookup name varlookup of
			Just (OModule m) -> 
				case argMap 
					(map ($varlookup) unnamed) 
					(map (\(a,b) -> (a, b varlookup)) named) m
				of
				(Just computationModifier, []) ->  
					computationModifier statements (return state)
				(Nothing, []) -> do
					putStrLn $ "Module " ++ name ++ " failed without a message"
					return state
				(Nothing, errs) -> do
					putStrLn $ "Module " ++ name ++ " failed with the following messages:"
					forM_ errs (\err -> putStrLn $ "  " ++ err)
					return state
				(Just computationModifier, errs) -> do
					putStrLn $ "Module " ++ name ++ " gave the following warnings:"
					forM_ errs (\err -> putStrLn $ "  " ++ err)
					computationModifier statements (return state)
			_ -> do
				putStrLn $ "module " ++ name ++ " is not in scope"
				return state



userModuleDeclaration = do
	string "module"
	many space;
	newModuleName <- variableSymb;
	many space;
	args <- moduleArgsUnitDecl
	many space;
	codeStatements <- suite
	return $ \ envIOWrappedState -> do
		(envVarlookup, envObj2s, envObj3s) <- envIOWrappedState
		let 
			newModule = OModule $  do 
				argVarlookupModifier <- args envVarlookup
				return $ \childrenStatements contextIOWrappedState -> do
					contextState@(contextVarLookup, contextObj2s, contextObj3s)
						<- contextIOWrappedState
					(_, childObj2s, childObj3s) <- runComputations 
						(return contextState)
						childrenStatements;
					let
						children = ONum $ fromIntegral  
							(length childObj2s + length childObj3s)
						child = OModule $ liftM (\statemod suite -> statemod) $ do
							n :: ℕ <- argument "n";
							if n <= length childObj3s 
							         then addObj3 (childObj3s !! n)
							         else addObj2 (childObj2s !! (n+1-length childObj3s))
						varlookupForCode = 
							(insert "child" child) $ 
							(insert "children" children) $
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



unionStatement = moduleWithSuite "union" $ \suite -> do
	r :: ℝ <- argument "r"
		`defaultTo` 0.0
	if r > 0
		then getAndCompressSuiteObjs suite (Prim.unionR r) (Prim.unionR r)
		else getAndCompressSuiteObjs suite Prim.union Prim.union

intersectStatement = moduleWithSuite "intersection" $ \suite -> do
	r :: ℝ <- argument "r"
		`defaultTo` 0.0
	if r > 0
		then getAndCompressSuiteObjs suite (Prim.intersectR r) (Prim.intersectR r)
		else getAndCompressSuiteObjs suite Prim.intersect Prim.intersect

differenceStatement = moduleWithSuite "difference" $ \suite -> do
	r :: ℝ <- argument "r"
		`defaultTo` 0.0
	if r > 0
		then getAndCompressSuiteObjs suite (Prim.differenceR r) (Prim.differenceR r)
		else getAndCompressSuiteObjs suite Prim.difference Prim.difference

translateStatement = moduleWithSuite "translate" $ \suite -> do
	v <- argument "v"
	caseOType v $
		       ( \(x,y,z)-> 
			getAndTransformSuiteObjs suite (Prim.translate (x,y) ) (Prim.translate (x,y,z)) 
		) <||> ( \(x,y) -> 
			getAndTransformSuiteObjs suite (Prim.translate (x,y) ) (Prim.translate (x,y,0.0)) 
		) <||> ( \ x -> 
			getAndTransformSuiteObjs suite (Prim.translate (x,0.0) ) (Prim.translate (x,0.0,0.0))
		) <||> (\ _  -> noChange)

deg2rad x = x / 180.0 * pi

-- This is mostly insane
rotateStatement = moduleWithSuite "rotate" $ \suite -> do
	a <- argument "a"
	caseOType a $
		       ( \xy  ->
			getAndTransformSuiteObjs suite (Prim.rotate $ deg2rad xy ) (Prim.rotate3 (deg2rad xy, 0, 0) )
		) <||> ( \(yz,xy,xz) ->
			getAndTransformSuiteObjs suite (Prim.rotate $ deg2rad xy ) (Prim.rotate3 (deg2rad yz, deg2rad xz, deg2rad xy) )
		) <||> ( \(yz,xz) ->
			getAndTransformSuiteObjs suite (id ) (Prim.rotate3 (deg2rad yz, deg2rad xz, 0))
		) <||> ( \_  -> noChange )


scaleStatement = moduleWithSuite "scale" $ \suite -> do
	v <- argument "v"
	case v of
		{-OList ((ONum x):(ONum y):(ONum z):[]) -> 
			getAndTransformSuiteObjs suite (Prim.translate (x,y) ) (Prim.translate (x,y,z))
		OList ((ONum x):(ONum y):[]) -> 
			getAndTransformSuiteObjs suite (Prim.translate (x,y) ) (Prim.translate (x,y,0.0))
		OList ((ONum x):[]) -> 
			getAndTransformSuiteObjs suite (Prim.translate (x,0.0) ) (Prim.translate (x,0.0,0.0)-}
		ONum s ->
			getAndTransformSuiteObjs suite (Prim.scale s) (Prim.scale s)

extrudeStatement = moduleWithSuite "linear_extrude" $ \suite -> do
	height :: ℝ   <- argument "height"
	center :: Bool<- argument "center" `defaultTo` False
	twist  :: Any <- argument "twist"  `defaultTo` (ONum 0)
	r      :: ℝ   <- argument "r"      `defaultTo` 0
	let
		degRotate = (\θ (x,y) -> (x*cos(θ)+y*sin(θ), y*cos(θ)-x*sin(θ))) . (*(2*pi/360))
		shiftAsNeeded =
			if center
			then Prim.translate (0,0,-height/2.0)
			else id
	caseOType twist $
		(\ (rot :: ℝ) ->
			getAndModUpObj2s suite $ \obj -> 
				shiftAsNeeded $ if rot == 0 
					then Prim.extrudeR    r                               obj height
					else Prim.extrudeRMod r (degRotate . (*(rot/height))) obj height
		) <||> (\ (rotf :: ℝ -> Maybe ℝ) ->
			getAndModUpObj2s suite $ \obj -> 
				shiftAsNeeded $ Prim.extrudeRMod r 
					(degRotate . (fromMaybe 0) . rotf) obj height
		) <||> (\_ -> noChange)

{-rotateExtrudeStatement = moduleWithSuite "rotate_extrude" $ \suite -> do
	h <- realArgument "h"
	center <- boolArgumentWithDefault "center" False
	twist <- realArgumentWithDefault 0.0
	r <- realArgumentWithDefault "r" 0.0
	getAndModUpObj2s suite (\obj -> Prim.extrudeRMod r (\θ (x,y) -> (x*cos(θ)+y*sin(θ), y*cos(θ)-x*sin(θ)) )  obj h) 
-}

shellStatement = moduleWithSuite "shell" $ \suite -> do
	w :: ℝ <- argument "w"
	getAndTransformSuiteObjs suite (Prim.shell w) (Prim.shell w)

-- Not a perenant solution! Breaks if can't pack.
packStatement = moduleWithSuite "pack" $ \suite -> do
	size :: ℝ2 <- argument "size"
	sep  :: ℝ  <- argument "sep"
	let
		pack2 objs = case Prim.pack2 size sep objs of
			Just a -> a
		pack3 objs = case Prim.pack3 size sep objs of
			Just a -> a
	getAndCompressSuiteObjs  suite pack2 pack3

