-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts, TypeSynonymInstances, UndecidableInstances, ScopedTypeVariables  #-}

import Graphics.Implicit.ExtOpenScad.Primitives (primitives)
import Graphics.Implicit.ExtOpenScad.Util.ArgParser

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

