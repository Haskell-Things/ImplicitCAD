-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts, TypeSynonymInstances, UndecidableInstances, ScopedTypeVariables  #-}

-- FIXME: this doesn't work. looks like it broke badly when ArgParser became a Monad.

import Graphics.Implicit.ExtOpenScad.Primitives (primitives)
-- import Graphics.Implicit.ExtOpenScad.Util.ArgParser()
import Graphics.Implicit.ExtOpenScad.Definitions (ArgParser(AP,APFailIf,APExample,APTest,APTerminator,APBranch))

import qualified Control.Exception as Ex (catch, SomeException)
import Control.Monad

isExample (ExampleDoc _ ) = True
isExample _ = False

isArgument (ArgumentDoc _ _ _) = True
isArgument _ = False

main = do
	let names = map fst primitives
	docs <- sequence $ map (getArgParserDocs.($ []).snd) primitives

	forM_ (zip names docs) $ \(moduleName, moduleDocList) -> do
		let
			examples = filter isExample moduleDocList
			arguments = filter isArgument moduleDocList
		putStrLn moduleName
		putStrLn (map (const '-') moduleName)
		putStrLn ""
		if not $ null examples then putStrLn "**Examples:**\n" else return ()
		forM_ examples $ \(ExampleDoc example) -> do
			putStrLn $ "   * `" ++ example ++ "`"
		putStrLn ""
		putStrLn "**Arguments:**\n"
		forM_ arguments $ \(ArgumentDoc name posfallback description) ->
			case (posfallback, description) of
				(Nothing, "") -> do
					putStrLn $ "   * `" ++ name  ++ "`"
				(Just fallback, "") -> do
					putStrLn $ "   * `" ++ name ++ " = " ++ fallback ++ "`"
				(Nothing, _) -> do
					putStrLn $ "   * `" ++ name ++ "`"
					putStrLn $ "     " ++ description
				(Just fallback, _) -> do
					putStrLn $ "   * `" ++ name ++ " = " ++ fallback ++ "`"
					putStrLn $ "     " ++ description
		putStrLn ""

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

getArgParserDocs (AP name fallback doc fnext) =
    do
        otherDocs <- Ex.catch (getArgParserDocs $ fnext undefined) (\(e :: Ex.SomeException) -> return [])
        return $ (ArgumentDoc name (fmap show fallback) doc):otherDocs

getArgParserDocs (APFailIf _ _ child) = getArgParserDocs child

getArgParserDocs (APExample str child) =
    do
        childResults <- getArgParserDocs child
        return $ (ExampleDoc str) : childResults

-- We try to look at as little as possible, to avoid the risk of triggering an error.
-- Yay laziness!

getArgParserDocs (APTest   _ _ child) = getArgParserDocs child

-- To look at this one would almost certainly be death (exception)
getArgParserDocs (APTerminator _) = return []

getArgParserDocs (APBranch children) = return []-- mapM getArgParserDocs children
