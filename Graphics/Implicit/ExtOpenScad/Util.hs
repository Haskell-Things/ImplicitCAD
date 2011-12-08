module Graphics.Implicit.ExtOpenScad.Util where

import Prelude hiding (lookup)
import Graphics.Implicit.Definitions
import Graphics.Implicit.ExtOpenScad.Definitions
import Graphics.Implicit.ExtOpenScad.Expressions
import Data.Map hiding (map,foldl,split,filter,null)
import qualified Data.List
import Text.ParserCombinators.Parsec 
import Text.ParserCombinators.Parsec.Expr
import Control.Monad (liftM)

data ArgParser a = ArgParser String (Maybe OpenscadObj) (OpenscadObj -> ArgParser a) | ArgParserTerminator a

instance Monad ArgParser where
	(ArgParser str fallback f) >>= g = ArgParser str fallback (\a -> (f a) >>= g)
	(ArgParserTerminator a) >>= g = g a
	return a = ArgParserTerminator a

argMap :: [OpenscadObj] -> [(String, OpenscadObj)] -> ArgParser a -> Maybe a
argMap _ _ (ArgParserTerminator a) = Just a
argMap (x:unnamedArgs) namedArgs (ArgParser _ _ f) = 
	argMap unnamedArgs namedArgs (f x)
argMap [] namedArgs (ArgParser str fallback f) = case Data.List.lookup str namedArgs of
	Just a -> argMap [] namedArgs (f a)
	Nothing -> case fallback of
		Just b -> argMap [] namedArgs (f b)
		Nothing -> Nothing

argument :: String -> ArgParser OpenscadObj
argument str = ArgParser str Nothing (\a -> return a)

argumentWithDefault :: String -> OpenscadObj -> ArgParser OpenscadObj

argumentWithDefault str fallback = ArgParser str (Just fallback) (\a -> return a)

addObj2 :: (Monad m) => Obj2 -> m ComputationStateModifier
addObj2 obj = return $ \(varlookup, obj2s, obj3s, io) -> (varlookup, obj2s ++ [obj], obj3s, io)

addObj3 :: (Monad m) => Obj3 -> m ComputationStateModifier
addObj3 obj = return $ \(varlookup, obj2s, obj3s, io) -> (varlookup, obj2s, obj3s ++ [obj], io)

noChange :: (Monad m) => m ComputationStateModifier
noChange = return id

moduleWithoutSuite :: 
	String -> ArgParser ComputationStateModifier -> GenParser Char st ComputationStateModifier

moduleWithoutSuite name argHandeler = do
	string name;
	many space;
	char '(';
	many space;
	args <- sepBy ( 
		try (do {
			symb <- variableSymb;
			many space;
			char '=';
			many space;
			expr <- expression 0;
			return $ Right (symb, expr);
		}) <|> (do {
			expr <- expression 0;
			return $ Left expr;
		})
		) (char ',');
	many space;	
	char ')';
	let
		named = map (\(Right a) -> a) $ filter (\(Right a) -> True) $ args
		unnamed = map (\(Left a) -> a) $ filter (\(Left a) -> True) $ args
		in return $ \state@(varlookup, obj2s, obj3s, io) -> 
			case argMap 
				(map ($varlookup) unnamed) 
				(map (\(a,b) -> (a, b varlookup)) named) argHandeler 
			of
				Just computationModifier ->  computationModifier state
				Nothing -> state;


pad parser = do
	many space
	a <- parser
	many space
	return a


