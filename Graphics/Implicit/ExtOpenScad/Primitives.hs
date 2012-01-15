-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

-- We'd like to parse openscad code, with some improvements, for backwards compatability.

module Graphics.Implicit.ExtOpenScad.Primitives where

import Prelude hiding (lookup)
import Graphics.Implicit.Definitions
import qualified Graphics.Implicit.Primitives as Prim
import Graphics.Implicit.ExtOpenScad.Definitions
import Graphics.Implicit.ExtOpenScad.Expressions
import Graphics.Implicit.ExtOpenScad.Util
import Data.Map hiding (map,foldl,split,filter,null)
import Text.ParserCombinators.Parsec 
import Text.ParserCombinators.Parsec.Expr
import Control.Monad (liftM)

sphere = moduleWithoutSuite "sphere" $ do
	r <- realArgument "r";
	addObj3 $ Prim.sphere r;

cube = moduleWithoutSuite "cube" $ do
	size <- argument "size";
	center <- boolArgumentWithDefault "center" False;
	case size of
		OList ((ONum x):(ONum y):(ONum z):[]) -> 
			if center  
			then addObj3 $ Prim.cubeVC (x, y, z)
			else addObj3 $ Prim.cubeV (x, y, z)
		ONum w -> 
			if center
			then addObj3 $ Prim.cubeC w
			else addObj3 $ Prim.cube w
		_ -> noChange;

-- What about $fn for regular n-gon prisms? This will break models..
cylinder = moduleWithoutSuite "cylinder" $ do
	h  <- realArgumentWithDefault "h"  1;
	r  <- realArgumentWithDefault "r"  1;
	r1 <- realArgumentWithDefault "r1" 1;
	r2 <- realArgumentWithDefault "r2" 1;
	center <- boolArgumentWithDefault "center" False;
	if r1 == 1 && r2 == 1
		then if center
			then addObj3 $ Prim.cylinderC r h
			else addObj3 $ Prim.cylinder  r h
		else if center
			then addObj3 $ Prim.cylinder2C r1 r2 h
			else addObj3 $ Prim.cylinder2  r1 r2 h


circle = moduleWithoutSuite "circle" $ do
	r  <- realArgument "r";
	fn <- intArgumentWithDefault "$fn" (-1);
	if fn < 3
		then addObj2 $ Prim.circle r
		else addObj2 $ Prim.polygon [(r*cos θ, r*sin θ )| θ <- [2*pi*n/fromIntegral fn | n <- [0.0 .. fromIntegral fn - 1.0]]]
		--else addObj2 $ Prim.regularPolygon fn r

square = moduleWithoutSuite "square" $ do
	size <- argument "size";
	center <- boolArgumentWithDefault "center" False;
	case size of
		OList ((ONum x):(ONum y):[]) -> 
			if center  
			then addObj2 $ Prim.squareVC (x, y)
			else addObj2 $ Prim.squareV (x, y)
		ONum w -> 
			if center
			then addObj2 $ Prim.squareC w
			else addObj2 $ Prim.square w
		_ -> noChange;


polygon = moduleWithoutSuite "polygon" $ do
	points <- argument "points";
	pathes <- argumentWithDefault "pathes" (OUndefined);
	let
		extractTupleList :: [OpenscadObj] -> Maybe [ℝ2]
		extractTupleList []  = Just []
		extractTupleList (OList ((ONum x):(ONum y):[]):others) = 
			case extractTupleList others of
				Just l -> Just $ (x,y):l
				Nothing -> Nothing
		extractTupleList _ = Nothing

		extractNumList :: [OpenscadObj] -> Maybe [ℝ]
		extractNumList [] = Just []
		extractNumList ((ONum n):others) = 
			case extractNumList others of
				Just l -> Just $ n:l
				Nothing -> Nothing
		extractNumList _ = Nothing

		in case (points, pathes) of
			(OList pointList, OUndefined) -> case extractTupleList pointList of
				Just tupleList -> addObj2 $ Prim.polygon tupleList
				Nothing -> noChange
			{-(OList pointList, OList pathList) -> 
				case (extractTupleList pointList, extractNumList pathList) of
					(Just l1, Just l2) -> -}
			_ -> noChange;


	
