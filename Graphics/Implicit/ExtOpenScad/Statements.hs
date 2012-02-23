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
import qualified Graphics.Implicit.Operations as Op
import Data.Map (Map, lookup, insert)
import Text.ParserCombinators.Parsec 
import Text.ParserCombinators.Parsec.Expr
import Control.Monad (liftM)
import Data.Maybe (fromMaybe)

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
			shellStatement,
			userModuleDeclaration
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
			useStatement,
			sphere,
			cube,
			square,
			cylinder,
			circle,
			polygon
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

-- | Run a list of computations!
--   We start with a state and run it through a bunch of ComputationStateModifier s.
runComputations :: ComputationState -> [ComputationStateModifier]  -> ComputationState
runComputations = foldl (\a b -> b $ a)

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
		varSymb <- variableSymb
		many space
		char '='
		many space
		valExpr <- expression 0
		return $ \ ioWrappedState -> do
			(varlookup, obj2s, obj3s) <- ioWrappedState
			let
				val = valExpr varlookup
			return (insert varSymb val varlookup, obj2s, obj3s) 
	) <|> (try $ do 
		varSymb <- variableSymb
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
	val <- expression 0
	many space
	char ')'
	return $  \ ioWrappedState -> do
		state@(varlookup, _, _) <- ioWrappedState
		putStrLn $ show $ val varlookup
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
		if case bexpr varlookup of  
				OBool b -> b
				_ -> False
			then runComputations (return state) statementsTrueCase
			else runComputations (return state) statementsFalseCase
	) <?> "if statement"

forStatement = (do
	-- a for loop is of the form:
	--      for ( vsymb = vexpr   ) loopStatements
	-- eg.  for ( a     = [1,2,3] ) {echo(a); echo "lol";}
	string "for"
	many space
	char '('
	many space
	vsymb <- variableSymb
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
				(varlookup, a, b) <- ioWrappedState;
				let
					vsymbSetState = return (insert vsymb val varlookup, a, b)
				runComputations vsymbSetState loopStatements
		-- Then loops once for every entry in vexpr
		foldl (loopOnce) (return state) $ case vexpr varlookup of 
			OList l -> l;
			_       -> [];
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
				Just computationModifier ->  computationModifier (return state)
				Nothing -> (return state);
	) <?> (name ++ " statement")


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
					Just computationModifier ->  
						computationModifier statements (return state)
					Nothing -> (return state);
			_ -> return state



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
			moduleArgParser :: ArgParser ([ComputationStateModifier] -> ComputationStateModifier)
			moduleArgParser =  do 
				argVarlookupModifier <- args envVarlookup
				return $ \childrenStatements contextIOWrappedState -> do
					(contextVarLookup, contextObj2s, contextObj3s)
						<- contextIOWrappedState
					(_, childObj2s, childObj3s) <- runComputations 
						(return (argVarlookupModifier contextVarLookup, [],[]) )
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
							envVarlookup
					(_, resultObj2s, resultObj3s) 
						<- runComputations 
							(return (varlookupForCode, [],[])) 
							codeStatements
					return (
						contextVarLookup, 
						contextObj2s ++ resultObj2s, 
						contextObj3s ++ resultObj3s
						)
		return (insert newModuleName (OModule moduleArgParser) envVarlookup, envObj2s, envObj3s)



getAndModUpObj2s :: (Monad m) => [ComputationStateModifier] 
	-> (Obj2Type -> Obj3Type)
	-> m ComputationStateModifier
getAndModUpObj2s suite obj2mod = 
	return $  \ ioWrappedState -> do
		(varlookup,  obj2s,  obj3s)  <- ioWrappedState
		(varlookup2, obj2s2, obj3s2) <- runComputations (return (varlookup, [], [])) suite
		return 
			(varlookup2,
			 obj2s, 
			 obj3s ++ (case obj2s2 of [] -> []; x:xs -> [obj2mod x])  )

getAndCompressSuiteObjs :: (Monad m) => [ComputationStateModifier] 
	-> ([Obj2Type] -> Obj2Type)
	-> ([Obj3Type] -> Obj3Type)
	-> m ComputationStateModifier
getAndCompressSuiteObjs suite obj2modifier obj3modifier = 
	return $  \ ioWrappedState -> do
		(varlookup,  obj2s,  obj3s)  <- ioWrappedState
		(varlookup2, obj2s2, obj3s2) <- runComputations (return (varlookup, [], [])) suite
		return 
			(varlookup2,
			 obj2s ++ (case obj2s2 of [] -> []; _ -> [obj2modifier obj2s2]), 
			 obj3s ++ (case obj3s2 of [] -> []; _ -> [obj3modifier obj3s2])  )

getAndTransformSuiteObjs :: (Monad m) => [ComputationStateModifier] 
	-> (Obj2Type -> Obj2Type)
	-> (Obj3Type -> Obj3Type)
	-> m ComputationStateModifier
getAndTransformSuiteObjs suite obj2modifier obj3modifier = 
	return $  \ ioWrappedState -> do
		(varlookup,  obj2s,  obj3s)  <- ioWrappedState
		(varlookup2, obj2s2, obj3s2) <- runComputations (return (varlookup, [], [])) suite
		return 
			(varlookup2,
			 obj2s ++ (map obj2modifier obj2s2),
			 obj3s ++ (map obj3modifier obj3s2)   )


unionStatement = moduleWithSuite "union" $ \suite -> do
	r :: ℝ <- argument "r"
		`defaultTo` 0.0
	if r > 0
		then getAndCompressSuiteObjs suite (Op.unionR r) (Op.unionR r)
		else getAndCompressSuiteObjs suite Op.union Op.union

intersectStatement = moduleWithSuite "intersection" $ \suite -> do
	r :: ℝ <- argument "r"
		`defaultTo` 0.0
	if r > 0
		then getAndCompressSuiteObjs suite (Op.intersectR r) (Op.intersectR r)
		else getAndCompressSuiteObjs suite Op.intersect Op.intersect

differenceStatement = moduleWithSuite "difference" $ \suite -> do
	r :: ℝ <- argument "r"
		`defaultTo` 0.0
	if r > 0
		then getAndCompressSuiteObjs suite (Op.differenceR r) (Op.differenceR r)
		else getAndCompressSuiteObjs suite Op.difference Op.difference

translateStatement = moduleWithSuite "translate" $ \suite -> do
	v <- argument "v"
	caseOType v $
		       ( \(x,y,z)-> 
			getAndTransformSuiteObjs suite (Op.translate (x,y) ) (Op.translate (x,y,z)) 
		) <||> ( \(x,y) -> 
			getAndTransformSuiteObjs suite (Op.translate (x,y) ) (Op.translate (x,y,0.0)) 
		) <||> ( \ x -> 
			getAndTransformSuiteObjs suite (Op.translate (x,0.0) ) (Op.translate (x,0.0,0.0))
		) <||> (\ _  -> noChange)

-- This is mostly insane
rotateStatement = moduleWithSuite "rotate" $ \suite -> do
	a <- argument "a"
	caseOType a $
		       ( \xy  -> 
			getAndTransformSuiteObjs suite (Op.rotateXY xy ) (Op.rotate3 (xy, 0, 0) )
		) <||> ( \(yz,xz,xy) -> 
			getAndTransformSuiteObjs suite (Op.rotateXY xy ) (Op.rotate3 (yz, xz, xy) )
		) <||> ( \(yz,xz) -> 
			getAndTransformSuiteObjs suite (id ) (Op.rotate3 (yz, xz, 0))
		) <||> ( \_  -> noChange )


scaleStatement = moduleWithSuite "scale" $ \suite -> do
	v <- argument "v"
	case v of
		{-OList ((ONum x):(ONum y):(ONum z):[]) -> 
			getAndTransformSuiteObjs suite (Op.translate (x,y) ) (Op.translate (x,y,z))
		OList ((ONum x):(ONum y):[]) -> 
			getAndTransformSuiteObjs suite (Op.translate (x,y) ) (Op.translate (x,y,0.0))
		OList ((ONum x):[]) -> 
			getAndTransformSuiteObjs suite (Op.translate (x,0.0) ) (Op.translate (x,0.0,0.0)-}
		ONum s ->
			getAndTransformSuiteObjs suite (Op.scale s) (Op.scale s)

extrudeStatement = moduleWithSuite "linear_extrude" $ \suite -> do
	height :: ℝ   <- argument "height"
	center :: Bool<- argument "center" `defaultTo` False
	twist  :: Any <- argument "twist"  `defaultTo` (ONum 0)
	r      :: ℝ   <- argument "r"      `defaultTo` 0
	let
		degRotate = (\θ (x,y) -> (x*cos(θ)+y*sin(θ), y*cos(θ)-x*sin(θ))) . (*(2*pi/360))
		shiftAsNeeded =
			if center
			then Op.translate (0,0,-height/2.0)
			else id
	caseOType twist $
		(\ (0::ℝ) -> getAndModUpObj2s suite (\obj -> shiftAsNeeded $ Op.extrudeR r obj height) 
		) <||> (\ (rot :: ℝ) ->
			getAndModUpObj2s suite $ \obj -> 
				shiftAsNeeded $ Op.extrudeRMod r 
					(degRotate . (*(rot/height))) obj height
		) <||> (\ (rotf :: ℝ -> Maybe ℝ) ->
			getAndModUpObj2s suite $ \obj -> 
				shiftAsNeeded $ Op.extrudeRMod r 
					(degRotate . (fromMaybe 0) . rotf) obj height
		) <||> (\_ -> noChange)

{-rotateExtrudeStatement = moduleWithSuite "rotate_extrude" $ \suite -> do
	h <- realArgument "h"
	center <- boolArgumentWithDefault "center" False
	twist <- realArgumentWithDefault 0.0
	r <- realArgumentWithDefault "r" 0.0
	getAndModUpObj2s suite (\obj -> Op.extrudeRMod r (\θ (x,y) -> (x*cos(θ)+y*sin(θ), y*cos(θ)-x*sin(θ)) )  obj h) 
-}

shellStatement = moduleWithSuite "shell" $ \suite -> do
	w :: ℝ <- argument "w"
	getAndTransformSuiteObjs suite (Op.shell w) (Op.shell w)

