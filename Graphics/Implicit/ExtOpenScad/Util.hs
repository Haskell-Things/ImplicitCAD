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

data ArgParser a = ArgParser String (Maybe OpenscadObj) (OpenscadObj -> ArgParser a) 
                 | ArgParserTerminator a 
                 | ArgParserFail

instance Monad ArgParser where
	(ArgParser str fallback f) >>= g = ArgParser str fallback (\a -> (f a) >>= g)
	(ArgParserTerminator a) >>= g = g a
	(ArgParserFail) >>= g = ArgParserFail
	return a = ArgParserTerminator a

argMap :: [OpenscadObj] -> [(String, OpenscadObj)] -> ArgParser a -> Maybe a
argMap _ _ (ArgParserTerminator a) = Just a
argMap _ _ ArgParserFail = Nothing
argMap (x:unnamedArgs) namedArgs (ArgParser _ _ f) = 
	argMap unnamedArgs namedArgs (f x)
argMap [] namedArgs (ArgParser str fallback f) = case Data.List.lookup str namedArgs of
	Just a -> argMap [] namedArgs (f a)
	Nothing -> case fallback of
		Just b -> argMap [] namedArgs (f b)
		Nothing -> Nothing

argument :: String -> ArgParser OpenscadObj
argument str = ArgParser str Nothing (\a -> return a)

realArgument :: String -> ArgParser ℝ
realArgument str = ArgParser str Nothing (\a -> case a of {(ONum a) -> return a; _ -> ArgParserFail;})

intArgument :: String -> ArgParser Int
intArgument str = ArgParser str Nothing (\a -> case a of {(ONum a) -> return (floor a); _ -> ArgParserFail;})

boolArgument :: String -> ArgParser Bool
boolArgument str = ArgParser str Nothing (\a -> case a of {(OBool a) -> return a; _ -> ArgParserFail;})

argumentWithDefault :: String -> OpenscadObj -> ArgParser OpenscadObj
argumentWithDefault str fallback = ArgParser str (Just fallback) (\a -> return a)

realArgumentWithDefault :: String -> ℝ -> ArgParser ℝ
realArgumentWithDefault str fallback = ArgParser str (Just (ONum fallback)) 
	(\a -> case a of {(ONum a) -> return a; _ -> ArgParserFail;})

intArgumentWithDefault :: String -> Int -> ArgParser Int
intArgumentWithDefault str fallback = ArgParser str (Just (ONum (fromIntegral fallback))) 
	(\a -> case a of {(ONum a) -> return (floor a); _ -> ArgParserFail;})

boolArgumentWithDefault :: String -> Bool -> ArgParser Bool
boolArgumentWithDefault str fallback = ArgParser str (Just (OBool fallback)) 
	(\a -> case a of {(OBool a) -> return a; _ -> ArgParserFail;})

addObj2 :: (Monad m) => Obj2Type -> m ComputationStateModifier
addObj2 obj = return $  \ ioWrappedState -> do
		(varlookup, obj2s, obj3s) <- ioWrappedState
		return (varlookup, obj2s ++ [obj], obj3s)

addObj3 :: (Monad m) => Obj3Type -> m ComputationStateModifier
addObj3 obj = return $  \ ioWrappedState -> do
		(varlookup, obj2s, obj3s) <- ioWrappedState
		return (varlookup, obj2s, obj3s ++ [obj])

changeObjs :: (Monad m) => ([Obj2Type] -> [Obj2Type]) -> ([Obj3Type] -> [Obj3Type]) -> m ComputationStateModifier
changeObjs mod2s mod3s = return $  \ ioWrappedState -> do
		(varlookup, obj2s, obj3s) <- ioWrappedState
		return (varlookup, mod2s obj2s, mod3s obj3s)

runIO ::  (Monad m) => IO() -> m ComputationStateModifier
runIO newio = return $  \ ioWrappedState -> do
		state <- ioWrappedState
		newio
		return state

noChange :: (Monad m) => m ComputationStateModifier
noChange = return id

moduleArgsUnit ::  
	GenParser Char st ([VariableLookup -> OpenscadObj], [(String, VariableLookup -> OpenscadObj)])
moduleArgsUnit = do
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
		) (many space >> char ',' >> many space);
	many space;	
	char ')';
	let
		isRight (Right a) = True
		isRight _ = False
		named = map (\(Right a) -> a) $ filter isRight $ args
		unnamed = map (\(Left a) -> a) $ filter (not . isRight) $ args
		in return (unnamed, named)


moduleWithoutSuite :: 
	String -> ArgParser ComputationStateModifier -> GenParser Char st ComputationStateModifier

moduleWithoutSuite name argHandeler = (do
	string name;
	many space;
	(unnamed, named) <- moduleArgsUnit
	return $ \ ioWrappedState -> do
		state@(varlookup, obj2s, obj3s) <- ioWrappedState
		case argMap 
			(map ($varlookup) unnamed) 
			(map (\(a,b) -> (a, b varlookup)) named) argHandeler 
			of
				Just computationModifier ->  computationModifier (return state)
				Nothing -> (return state);
	) <?> name


pad parser = do
	many space
	a <- parser
	many space
	return a


