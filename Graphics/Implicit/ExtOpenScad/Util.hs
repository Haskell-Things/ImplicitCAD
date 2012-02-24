-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

-- We'd like to parse openscad code, with some improvements, for backwards compatability.


{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts, TypeSynonymInstances, UndecidableInstances, ScopedTypeVariables  #-}

module Graphics.Implicit.ExtOpenScad.Util where

import Prelude hiding (lookup)
import Graphics.Implicit.Definitions
import Graphics.Implicit.ExtOpenScad.Definitions
import Graphics.Implicit.ExtOpenScad.Expressions
import Data.Map (Map, lookup, insert)
import qualified Data.List
import Text.ParserCombinators.Parsec 
import Text.ParserCombinators.Parsec.Expr
import Data.Maybe (isJust,isNothing)
import qualified Control.Exception as Ex

instance Monad ArgParser where
	(ArgParser str fallback doc f) >>= g = ArgParser str fallback doc (\a -> (f a) >>= g)
	(ArgParserTerminator a) >>= g = g a
	(ArgParserFailIf b errmsg child) >>= g = ArgParserFailIf b errmsg (child >>= g)
	return a = ArgParserTerminator a

argMap :: [OpenscadObj] -> [(String, OpenscadObj)] -> ArgParser a -> Maybe a
argMap _ _ (ArgParserTerminator a) = Just a
argMap a b (ArgParserFailIf test err child) = if test then Nothing else argMap a b child
argMap (x:unnamedArgs) namedArgs (ArgParser _ _ _ f) = 
	argMap unnamedArgs namedArgs (f x)
argMap [] namedArgs (ArgParser str fallback _ f) = case Data.List.lookup str namedArgs of
	Just a -> argMap [] namedArgs (f a)
	Nothing -> case fallback of
		Just b -> argMap [] namedArgs (f b)
		Nothing -> Nothing

-- $ Here there be dragons!
--   We give undefined (= an error) and let laziness prevent if from ever being touched.
--   We're using IO so that we can catch an error if this backfires.
--   If so, we *back off*.
getArgParserDocs :: (ArgParser a) -> IO [(String, Maybe String, String)]
getArgParserDocs (ArgParser name fallback doc fnext) = do
	otherDocs <- Ex.catch (getArgParserDocs $ fnext undefined) (\(e :: Ex.SomeException) -> return [])
	return $ (name, fmap show fallback, doc):otherDocs
-- We try to look at as little as possible, to avoid the risk of triggering an error.
-- Yay laziness!
getArgParserDocs (ArgParserFailIf _ _ child ) = getArgParserDocs child
-- To look at this one would almost certainly be 
getArgParserDocs (ArgParserTerminator _ ) = return []

argument :: forall desiredType. (OTypeMirror desiredType) => String -> ArgParser desiredType
argument name = 
	ArgParser name Nothing "" $ \oObjVal -> do
		let
			val = fromOObj oObjVal :: Maybe desiredType
			errmsg = "arg " ++ show oObjVal ++ " not compatible with " ++ name
		-- Using /= Nothing would require Eq desiredType
		ArgParserFailIf (isNothing val) errmsg $ ArgParserTerminator $ (\(Just a) -> a) val

type Any = OpenscadObj

caseOType = flip ($)

doc (ArgParser name defMaybeVal oldDoc next) doc =
	ArgParser name defMaybeVal doc next

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

defaultTo :: forall a. (OTypeMirror a) => ArgParser a -> a -> ArgParser a
defaultTo (ArgParser name oldDefMaybeVal doc next) newDefVal = 
	ArgParser name (Just $ toOObj newDefVal) doc next


addObj2 :: (Monad m) => Obj2Type -> m ComputationStateModifier
addObj2 obj = return $  \ ioWrappedState -> do
		(varlookup, obj2s, obj3s) <- ioWrappedState
		return (varlookup, obj:obj2s, obj3s)

addObj3 :: (Monad m) => Obj3Type -> m ComputationStateModifier
addObj3 obj = return $  \ ioWrappedState -> do
		(varlookup, obj2s, obj3s) <- ioWrappedState
		return (varlookup, obj2s, obj:obj3s)

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


