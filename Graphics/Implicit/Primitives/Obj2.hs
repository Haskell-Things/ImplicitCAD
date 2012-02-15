-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts, TypeSynonymInstances, UndecidableInstances #-}

module Graphics.Implicit.Primitives.Obj2 where

import Graphics.Implicit.Definitions
import qualified Graphics.Implicit.MathUtil as MathUtil
import Graphics.Implicit.Primitives.Definitions
import qualified Graphics.Implicit.SaneOperators as S
import Data.List (nub)


instance PrimitiveSupporter2 Obj2 where

	circle r = \(x,y) -> sqrt (x**2 + y**2) - r

	rectR r (x1,y1) (x2,y2) = 
		\(x,y) -> MathUtil.rmaximum  r [abs (x- dx/2.0 -x1) -dx/2.0, abs (y- dy/2.0 -y1) - dy/2.0]
			where (dx,dy) = (x2-x1,y2-y1)

	polygonR 0 points p@(p1,p2) = minimum dists * if isIn then -1 else 1
		where
			pair n = (points !! n, points !! (mod (n+1) (length points) ) )
			pairs =  [ pair n | n <- [0 .. (length points) - 1] ]
			relativePairs =  map (\(a,b) -> (a S.- p, b S.- p) ) pairs
			crossing_points =
				[x2 - y2*(x2-x1)/(y2-y1) | ((x1,y1), (x2,y2)) <-relativePairs,
				   ( (y2 <= 0) && (y1 >= 0) ) || ( (y2 >= 0) && (y1 <= 0) ) ]
			seemsInRight = odd $ length $ filter (>0) $ nub crossing_points
			seemsInLeft = odd $ length $ filter (<0) $ nub crossing_points
			isIn = seemsInRight && seemsInLeft
			dists = map (MathUtil.distFromLineSeg p) pairs

