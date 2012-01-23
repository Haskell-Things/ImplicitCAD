-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts, TypeSynonymInstances, UndecidableInstances #-}

module Graphics.Implicit.Primitives.Obj2 where

import Graphics.Implicit.Definitions
import Graphics.Implicit.Primitives.Definitions
import qualified Graphics.Implicit.SaneOperators as S


instance PrimitiveSupporter2 Obj2 where

	circle r = \(x,y) -> sqrt (x**2 + y**2) - r

	squareV (dx,dy) = \(x,y) -> (maximum [abs (x- dx/2.0) -dx, abs (y- dy/2.0) - dy])

	polygon points =
		let
			pairs =
			   [ (points !! n, points !! (mod (n+1) (length points) ) ) | n <- [0 .. (length points) - 1] ]
			isIn p@(p1,p2) =
				let
					crossing_points =
						[x1 + (x2-x1)*y2/(y2-y1) |
						((x1,y1), (x2,y2)) <-
							map (\((a1,a2),(b1,b2)) -> ((a1-p1,a2-p2), (b1-p1,b2-p2)) ) pairs,
						( (y2 < 0) && (y1 > 0) ) || ( (y2 > 0) && (y1 < 0) ) ]
				in
					if odd $ length $ filter (>0) crossing_points then -1 else 1
			dist a@(a1,a2) b@(b1,b2) p@(p1,p2) =
				let
					ab = b S.- a
					nab = (1 / S.norm ab) S.* ab
					ap = p S.- a
					d  = nab S.⋅ ap
					closest
						| d < 0 = a
						| d > S.norm ab = b
						| otherwise = a S.+ d S.* nab
				in
					S.norm (closest S.- p)
			dists = \ p -> map (\(a,b) ->  dist a b p) pairs
		in
			\ p -> isIn p * minimum (dists p)

