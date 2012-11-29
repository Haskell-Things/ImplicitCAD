
{-# LANGUAGE ViewPatterns, RankNTypes, ScopedTypeVariables #-}
module Graphics.Implicit.ExtOpenScad.Util.ArgParser where

import Graphics.Implicit.Definitions
import Graphics.Implicit.ExtOpenScad.Definitions
import Graphics.Implicit.ExtOpenScad.Util.OVal
import qualified Control.Exception as Ex
import qualified Data.Map   as Map
import qualified Data.Maybe as Maybe

instance Monad ArgParser where

	-- return is easy: if we want an ArgParser that just gives us a, that is 
	-- ArgParserTerminator a
	return a = ArgParserTerminator a

	-- Now things get more interesting. We need to describe how (>>=) works.
	-- Let's get the hard ones out of the way first.
	-- ArgParser actually 
	(ArgParser str fallback doc f) >>= g = ArgParser str fallback doc (\a -> (f a) >>= g)
	(ArgParserFailIf b errmsg child) >>= g = ArgParserFailIf b errmsg (child >>= g)
	-- These next to is easy, they just pass the work along to their child
	(ArgParserExample str child) >>= g = ArgParserExample str (child >>= g)
	(ArgParserTest str tests child) >>= g = ArgParserTest str tests (child >>= g)
	-- And an ArgParserTerminator happily gives away the value it contains
	(ArgParserTerminator a) >>= g = g a

-- * ArgParser building functions

-- ** argument and combinators

argument :: forall desiredType. (OTypeMirror desiredType) => String -> ArgParser desiredType
argument name = 
	ArgParser name Nothing "" $ \oObjVal -> do
		let
			val = fromOObj oObjVal :: Maybe desiredType
			errmsg = case oObjVal of
				OError errs -> "error in computing value for arugment " ++ name
				             ++ ": " ++ concat errs
				_   ->  "arg " ++ show oObjVal ++ " not compatible with " ++ name
		-- Using /= Nothing would require Eq desiredType
		ArgParserFailIf (Maybe.isNothing val) errmsg $ ArgParserTerminator $ (\(Just a) -> a) val

doc (ArgParser name defMaybeVal oldDoc next) doc =
	ArgParser name defMaybeVal doc next

defaultTo :: forall a. (OTypeMirror a) => ArgParser a -> a -> ArgParser a
defaultTo (ArgParser name oldDefMaybeVal doc next) newDefVal = 
	ArgParser name (Just $ toOObj newDefVal) doc next

-- ** example

example :: String -> ArgParser ()
example str = ArgParserExample str (return ())

-- * test and combinators

test :: String -> ArgParser ()
test str = ArgParserTest str [] (return ())

eulerCharacteristic :: ArgParser a -> Int -> ArgParser a
eulerCharacteristic (ArgParserTest str tests child) χ =
	ArgParserTest str ((EulerCharacteristic χ) : tests) child

-- * Tools for handeling ArgParsers

-- | Apply arguments to an ArgParser

argMap :: 
	[(Maybe String,  OVal)]      -- ^ arguments
	-> ArgParser a              -- ^ ArgParser to apply them to
	-> (Maybe a, [String])      -- ^ (result, error messages)

argMap args = argMap2 unnamedArgs (Map.fromList namedArgs) where
	unnamedArgs = map snd $ filter (Maybe.isNothing . fst) args
	namedArgs   = map (\(a,b) -> (Maybe.fromJust a, b)) $ filter (Maybe.isJust . fst) args


argMap2 :: [OVal] -> Map.Map String OVal -> ArgParser a -> (Maybe a, [String])

argMap2 unnamedArgs namedArgs (ArgParser name fallback _ f) = 
	case Map.lookup name namedArgs of
		Just a -> argMap2 
			unnamedArgs 
			(Map.delete name namedArgs) 
			(f a)
		Nothing -> case unnamedArgs of
			x:xs -> argMap2 xs namedArgs (f x)
			[]   -> case fallback of
				Just b  -> argMap2 [] namedArgs (f b)
				Nothing -> (Nothing, ["No value and no default for argument " ++ name])

argMap2 a b (ArgParserTerminator val) = 
	(Just val,
		if length a + Map.size b > 0
		then ["unused arguments"]
		else []
	)

argMap2 a b (ArgParserFailIf test err child) = 
	if test 
	then (Nothing, [err])
	else argMap2 a b child

argMap2 a b (ArgParserExample str child) = argMap2 a b child

argMap2 a b (ArgParserTest str tests child) = argMap2 a b child


-- | We need a format to extract documentation into
data Doc = Doc String [DocPart]
             deriving (Show)

data DocPart = ExampleDoc String
             | ArgumentDoc String (Maybe String) String
             deriving (Show)


--   Here there be dragons!
--   Because we made this a Monad instead of applicative functor, there's now sane way to do this.
--   We give undefined (= an error) and let laziness prevent if from ever being touched.
--   We're using IO so that we can catch an error if this backfires.
--   If so, we *back off*.

-- | Extract Documentation from an ArgParser

getArgParserDocs :: 
	(ArgParser a)    -- ^ ArgParser
	-> IO [DocPart]  -- ^ Docs (sadly IO wrapped)

getArgParserDocs (ArgParser name fallback doc fnext) = 
	do
		otherDocs <- Ex.catch (getArgParserDocs $ fnext undefined) (\(e :: Ex.SomeException) -> return [])
		return $ (ArgumentDoc name (fmap show fallback) doc):otherDocs

getArgParserDocs (ArgParserExample str child) =
	do
		childResults <- getArgParserDocs child
		return $ (ExampleDoc str) : childResults

-- We try to look at as little as possible, to avoid the risk of triggering an error.
-- Yay laziness!

getArgParserDocs (ArgParserTest   _ _ child ) = getArgParserDocs child
getArgParserDocs (ArgParserFailIf _ _ child ) = getArgParserDocs child

-- To look at this one would almost certainly be death (exception)
getArgParserDocs (ArgParserTerminator _ ) = return []


