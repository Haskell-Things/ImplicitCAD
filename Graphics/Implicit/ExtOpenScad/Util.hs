-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

-- We'd like to parse openscad code, with some improvements, for backwards compatability.


{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts, TypeSynonymInstances, UndecidableInstances, ScopedTypeVariables  #-}

module Graphics.Implicit.ExtOpenScad.Util where

import Prelude hiding (lookup)
import Graphics.Implicit.Definitions
import Graphics.Implicit.ExtOpenScad.Definitions
import Graphics.Implicit.ExtOpenScad.Expressions
import Graphics.Implicit.ExtOpenScad.Util.ArgParser
import Data.Map (Map, lookup, insert)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.List
import Text.ParserCombinators.Parsec 
import Text.ParserCombinators.Parsec.Expr
import Data.Maybe (isJust,isNothing)
import Control.Monad (forM_)


type Any = OpenscadObj

caseOType = flip ($)

infixr 2 <||>

(<||>) :: forall desiredType out. (OTypeMirror desiredType)
	=> (desiredType -> out) 
	-> (OpenscadObj -> out)
	-> (OpenscadObj -> out)

(<||>) f g = \input ->
	let
		coerceAttempt = fromOObj input :: Maybe desiredType
	in 
		if isJust coerceAttempt -- ≅ (/= Nothing) but no Eq req
		then f $ (\(Just a) -> a) coerceAttempt
		else g input

moduleArgsUnit ::  
	GenParser Char st ([VariableLookup -> OpenscadObj], [(String, VariableLookup -> OpenscadObj)])
moduleArgsUnit = do
	char '(';
	many space;
	args <- sepBy ( 
		(try $ do -- eg. a = 12
			symb <- variableSymb;
			many space;
			char '=';
			many space;
			expr <- expression 0;
			return $ Right (symb, expr);
		) <|> (try $ do -- eg. a(x,y) = 12
			symb <- variableSymb;
			many space;
			char '('
			many space
			argVars <- sepBy variableSymb (many space >> char ',' >> many space)
			char ')'
			many space
			char '=';
			many space;
			expr <- expression 0;
			let
				makeFunc baseExpr (argVar:xs) varlookup' = OFunc $ 
					\argObj -> makeFunc baseExpr xs (insert argVar argObj varlookup')
				makeFunc baseExpr [] varlookup' = baseExpr varlookup'
				funcExpr = makeFunc expr argVars
			return $ Right (symb, funcExpr);
		) <|> (do { -- eg. 12
			expr <- expression 0;
			return $ Left expr;
		})
		) (many space >> char ',' >> many space);
	many space;	
	char ')';
	let
		isRight (Right a) = True
		isRight _ = False
		named = map (\(Right a) -> a) $ filter isRight $ args
		unnamed = map (\(Left a) -> a) $ filter (not . isRight) $ args
		in return (unnamed, named)

moduleArgsUnitDecl ::  
	GenParser Char st (VariableLookup -> ArgParser (VariableLookup -> VariableLookup))
moduleArgsUnitDecl = do
	char '(';
	many space;
	args <- sepBy ( 
		(try $ do
			symb <- variableSymb;
			many space;
			char '=';
			many space;
			expr <- expression 0;
			return $ \varlookup -> 
				ArgParser symb (Just$ expr varlookup) "" (\val -> return $ insert symb val);
		) <|> (try $ do
			symb <- variableSymb;
			many space;
			char '('
			many space
			argVars <- sepBy variableSymb (many space >> char ',' >> many space)
			char ')'
			many space
			char '=';
			many space;
			expr <- expression 0;
			let
				makeFunc baseExpr (argVar:xs) varlookup' = OFunc $ 
					\argObj -> makeFunc baseExpr xs (insert argVar argObj varlookup')
				makeFunc baseExpr [] varlookup' = baseExpr varlookup'
				funcExpr = makeFunc expr argVars
			return $ \varlookup ->
 				ArgParser symb (Just$ funcExpr varlookup) "" (\val -> return $ insert symb val);
		) <|> (do {
			vsymb <- variableSymb;
			return $ \varlookup ->
 				ArgParser vsymb Nothing "" (\val -> return $ insert vsymb val);
		})
		) (many space >> char ',' >> many space);
	many space;	
	char ')';
	let
		merge :: 
			(ArgParser (VariableLookup -> VariableLookup))
			->  (ArgParser (VariableLookup -> VariableLookup))
			->  (ArgParser (VariableLookup -> VariableLookup))
		merge a b = do
			a' <- a
			b' <- b
			return (b'.a')
	return $ \varlookup -> foldl merge (return id) $ map ($varlookup) $ args


pad parser = do
	many space
	a <- parser
	many space
	return a



patternMatcher :: GenParser Char st (OpenscadObj -> Maybe VariableLookup)
patternMatcher =
	(do 
		char '_'
		return (\obj -> Just Map.empty)
	) <|> ( do
		a <- literal
		return $ \obj ->
			if obj == (a undefined)
			then Just (Map.empty)
			else Nothing
	) <|> ( do
		symb <- variableSymb
		return $ \obj -> Just $ Map.singleton symb obj
	) <|> ( do
		char '['
		many space
		components <- patternMatcher `sepBy` (many space >> char ',' >> many space)
		many space
		char ']'
		return $ \obj -> case obj of
			OList l -> 
				if length l == length components
				then fmap Map.unions $ sequence $ zipWith ($) components l
				else Nothing
			_ -> Nothing
	)

