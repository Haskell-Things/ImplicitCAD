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

pad parser = do
	many space
	a <- parser
	many space
	return a

argMap :: [(String, Maybe a, a -> Bool)] -> [(String, a)] -> [a] -> Maybe [(String, a)]
argMap argFormat namedArgs unnamedArgs =
	let
		isNone Nothing = True
		isNone _ = False
		split test list = (filter test list, filter (not . test) list )
		bimap f (l1, l2) = (map f l1, map f l2)
		
		decideNamedArg :: ([(String,b)], [(String, Maybe b,  b -> Bool)] ) -> 
			(String,b) -> ([(String,b)], [(String, Maybe b,  b -> Bool)] )
		decideNamedArg (decidedArgs, presFormat) (name, val) = 
			case split (\(a,_,_)-> a == name) presFormat of
				([],_) -> (decidedArgs, presFormat)
				((_,_,test):[], remainder) ->
					if test val 
					then ((name,val):decidedArgs, remainder) 
					else (decidedArgs, presFormat)
		
		decideUnnamedArg :: ([(String,b)], [(String, Maybe b,  b -> Bool)] ) -> 
			b -> ([(String,b)], [(String, Maybe b,  b -> Bool)] )
		decideUnnamedArg (decidedArgs, []) _ = (decidedArgs, [])
		decideUnnamedArg (decidedArgs, presFormat@((nVar,_,nTest):others)) val = 
			if nTest val 
			then ((nVar, val):decidedArgs, others)
			else (decidedArgs, others)
		
		(nameClaimed, postNameFormat)= foldl decideNamedArg ([], argFormat) namedArgs
		(unnamedClaimed, notClaimed) = foldl decideUnnamedArg ([], postNameFormat) unnamedArgs
		(defaulted, noDefault) = bimap (\(a,Just b,_) -> (a,b) )  $  
			split (\(_,a,_) -> not $ isNone a) notClaimed
		result = nameClaimed ++ unnamedClaimed -- ++ defaulted
	in 
		if null noDefault
		then Just result
		else Nothing

{-plookup l i = case Data.List.lookup l i of
	Just n -> n

possiblyNamedArg = 
	try (do 
		name <- variableSymb
		many space
		char '='
		many space
		val <- expression 0
		return Left (name, val)
	) <|> (do
		val <- expression 0;
		return Right val
	)

possiblyNameArgs = do
	args <- sepBy possiblyNamedArg (char ',') 
	return $
		let
			isLeft Left a = True
			isLeft _ = False
			named = map (\Left a -> a) $ filter isLeft args
			unnamed = map (\Right a -> a) $ filter (not.isLeft) args
		in (named, unnamed)

-}
