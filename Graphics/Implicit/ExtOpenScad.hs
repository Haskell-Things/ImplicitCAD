-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

-- We'd like to parse openscad code, with some improvements, for backwards compatability.

module Graphics.Implicit.ExtOpenScad where

import Graphics.Implicit.Definitions
import Graphics.Implicit.ExtOpenScad.Definitions
import Graphics.Implicit.ExtOpenScad.Expressions
import Graphics.Implicit.ExtOpenScad.Default
import Graphics.Implicit.ExtOpenScad.Statements

import Prelude hiding (lookup)
import Data.Map hiding (map,foldl)
import Text.ParserCombinators.Parsec 
import Text.ParserCombinators.Parsec.Expr
import Control.Monad (liftM)


runComputationsDefault = runComputations
	(fromList funcs, [], [], return () )



parseComputations str = let 
		test :: Either ParseError [(ComputationState -> ComputationState)] -> IO()
		test (Right res) = case runComputationsDefault res of
			(varlookup, obj2s, obj3s, io) -> do
				io
				putStrLn "== DONE! =="
				putStrLn $ "2D Objs: " ++ show (length obj2s)
				putStrLn $ "3D Objs: " ++ show (length obj3s)
				putStrLn $ "varlookup:\n" ++ show (varlookup)
		test (Left err) =  putStrLn $ show $ err
	in test $ parse (many1 computationStatement) ""  str

funcs = [
		("sin", numericOFunc sin),
		("cos", numericOFunc cos),
		("tan", numericOFunc tan),
		("abs", numericOFunc abs),
		("sign", numericOFunc signum),
		("floor", numericOFunc (fromIntegral . floor) ),
		("ceil", numericOFunc (fromIntegral . ceiling) ),
		("exp", numericOFunc exp)
	]
