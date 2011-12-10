-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

-- We'd like to parse openscad code, with some improvements, for backwards compatability.

module Graphics.Implicit.ExtOpenScad.Statements where

import Prelude hiding (lookup)
import Graphics.Implicit.Definitions
import Graphics.Implicit.ExtOpenScad.Definitions
import Graphics.Implicit.ExtOpenScad.Expressions
import Graphics.Implicit.ExtOpenScad.Util
import Graphics.Implicit.ExtOpenScad.Primitives
import qualified Graphics.Implicit.Operations as Op
import Data.Map hiding (map,foldl)
import Text.ParserCombinators.Parsec 
import Text.ParserCombinators.Parsec.Expr
import Control.Monad (liftM)




assigmentStatement = do
	var <- variableSymb
	many space
	char '='
	many space
	val <- expression 0
	return $ \ (varlookup, obj2s, obj3s, io) -> (insert var (val varlookup) varlookup, obj2s, obj3s, io) 

echoStatement = do
	string "echo"
	many space
	char '('
	many space
	val <- expression 0
	many space
	char ')'
	return $ \(varlookup, obj2s, obj3s, io) -> (varlookup, obj2s, obj3s, io>>(putStrLn $ show $ val varlookup) ) 

suite :: GenParser Char st [ComputationStateModifier]
suite = liftM return computationStatement <|> do 
	char '{'
	stmts <- many computationStatement
	char '}'
	return stmts

ifStatement = do
	string "if"
	many space
	char '('
	bexpr <- expression 0
	char ')'
	many space
	statementsTrueCase <- suite
	many space
	statementsFalseCase <- try (string "else" >> many space >> suite ) <|> (return [])
	return $ \ (state@(varlookup, _, _, _)) -> if 
		case bexpr varlookup of  
			OBool b -> b
			_ -> False
		then runComputations state statementsTrueCase
		else runComputations state statementsFalseCase

forStatement = do
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
	return $ \ state@(varlookup,_,_,_) -> 
		let
			loopOnce (varlookup, a, b, c) val =  
				runComputations (insert vsymb val varlookup, a, b, c) loopStatements
		in
			foldl (loopOnce) state $ case vexpr varlookup of
				OList l -> l
				_       -> []

computationStatement :: GenParser Char st ComputationStateModifier
computationStatement = (many space >>)$  
	(try ifStatement 
	<|> try forStatement 
	<|> try unionStatement
	<|> try intersectStatement
	<|> try differenceStatement
	) 
	<|> do
		s <- try echoStatement <|> try assigmentStatement <|> try sphere <|> try cube
		many space
		char ';'
		return s


runComputations :: ComputationState -> [ComputationStateModifier]  -> ComputationState
runComputations = foldl ( \a b -> b $ a) 

moduleWithSuite ::
	String -> ([ComputationStateModifier] -> ArgParser ComputationStateModifier)
	-> GenParser Char st ComputationStateModifier
moduleWithSuite name argHandeler = do
	string name;
	many space;
	(unnamed, named) <- moduleArgsUnit
	statements <- suite
	return $ \state@(varlookup, obj2s, obj3s, io) -> 
		case argMap 
			(map ($varlookup) unnamed) 
			(map (\(a,b) -> (a, b varlookup)) named) (argHandeler statements)
		of
			Just computationModifier ->  computationModifier state
			Nothing -> state;


getAndCompressSuiteObjs :: (Monad m) => [ComputationStateModifier] 
	-> ([Obj2] -> Obj2)
	-> ([Obj3] -> Obj3)
	-> m ComputationStateModifier
getAndCompressSuiteObjs suite obj2modifier obj3modifier = 
	return $ \(varlookup, obj2s, obj3s, io) -> 
		(\(varlookup2, obj2s2, obj3s2, io2) -> 
			(varlookup2, 
			 obj2s ++ (case obj2s2 of [] -> []; _ -> [obj2modifier obj2s2]), 
			 obj3s ++ (case obj3s2 of [] -> []; _ -> [obj3modifier obj3s2]), 
			 io2)
		)
		(runComputations (varlookup, [], [], io) suite)

unionStatement = moduleWithSuite "union" $ \suite -> do
	r <- realArgumentWithDefault "r" 0.0
	if r > 0
	then getAndCompressSuiteObjs suite (Op.unionR r) (Op.unionR r)
	else getAndCompressSuiteObjs suite Op.union Op.union

intersectStatement = moduleWithSuite "intersection" $ \suite -> do
	r <- realArgumentWithDefault "r" 0.0
	if r > 0
	then getAndCompressSuiteObjs suite (Op.intersectR r) (Op.intersectR r)
	else getAndCompressSuiteObjs suite Op.intersect Op.intersect

differenceStatement = moduleWithSuite "difference" $ \suite -> do
	r <- realArgumentWithDefault "r" 0.0
	if r > 0
	then getAndCompressSuiteObjs suite (Op.differenceR r) (Op.differenceR r)
	else getAndCompressSuiteObjs suite Op.difference Op.difference

