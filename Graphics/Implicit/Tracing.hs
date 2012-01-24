-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

module Graphics.Implicit.Tracing (
	getTriangles,
	getLineSeg,
	orderLines,
	orderLinesDC,
	orderLinesP,
	reducePolyline,
	polylineNotNull,
	getMesh,
	getMesh2,
	getContour
) where

import Graphics.Implicit.Definitions
import Graphics.Implicit.Tracing.GetTriangles (getTriangles)

import Control.Parallel (par, pseq)

getLineSeg :: (ℝ,ℝ,ℝ,ℝ,ℝ,ℝ2,ℝ) -> [Polyline]
getLineSeg (x1y1,x2y1,x2y2,x1y2,c,(x,y),s) = 
	let 
		x1 = (x, y+s*x1y1/(x1y1-x1y2))
		x2 = (x+s, y+s*x2y1/(x2y1-x2y2))
		y1 = (x+s*x1y1/(x1y1-x2y1), y )
		y2 = (x+s*x1y2/(x1y2-x2y2), y+s)
		notPointLine (p1:p2:[]) = p1 /= p2
	in filter (notPointLine) $ case (x1y2 <= 0, x2y2 <= 0,
	         x1y1 <= 0, x2y1 <= 0) of
		(True,  True, 
		 True,  True)  -> []
		(False, False,
		 False, False) -> []
		(True,  True, 
		 False, False) -> [[x1, x2]]
		(False, False,
		 True,  True)  -> [[x1, x2]]
		(False, True, 
		 False, True)  -> [[y1, y2]]
		(True,  False,
		 True,  False) -> [[y1, y2]]
		(True,  False,
		 False, False) -> [[x1, y2]]
		(False, True, 
		 True,  True)  -> [[x1, y2]]
		(True,  True, 
		 False, True)  -> [[x1, y1]]
		(False, False,
		 True,  False) -> [[x1, y1]]
		(True,  True, 
		 True,  False) -> [[x2, y1]]
		(False, False,
		 False, True)  -> [[x2, y1]]
		(True,  False,
		 True,  True)  -> [[x2, y2]]
		(False, True, 
		 False, False) -> [[x2, y2]]
		(True,  False,
		 False, True)  -> if c > 0
			then [[x1, y2], [x2, y1]]
			else [[x1, y1], [x2, y2]]
		(False, True, 
		 True,  False) -> if c <= 0
			then [[x1, y2], [x2, y1]]
			else [[x1, y1], [x2, y2]]




orderLines :: [Polyline] -> [Polyline]
orderLines [] = []
orderLines (present:remaining) =
	let
		findNext ((p3:ps):segs) = if p3 == last present then (Just (p3:ps), segs) else
			if last ps == last present then (Just (reverse $ p3:ps), segs) else
			case findNext segs of (res1,res2) -> (res1,(p3:ps):res2)
		findNext [] = (Nothing, [])
	in
		case findNext remaining of
			(Nothing, _) -> present:(orderLines remaining)
			(Just match, others) -> orderLines $ (present ++ tail match): others

reducePolyline ((x1,y1):(x2,y2):(x3,y3):others) = 
	if (x1,y1) == (x2,y2) then reducePolyline ((x2,y2):(x3,y3):others) else
	if abs ( (y2-y1)/(x2-x1) - (y3-y1)/(x3-x1) ) < 0.0001 
	   || ( (x2-x1) == 0 && (x3-x1) == 0 && (y2-y1)*(y3-y1) > 0)
	then reducePolyline ((x1,y1):(x3,y3):others)
	else (x1,y1) : reducePolyline ((x2,y2):(x3,y3):others)
reducePolyline ((x1,y1):(x2,y2):others) = 
	if (x1,y1) == (x2,y2) then reducePolyline ((x2,y2):others) else (x1,y1):(x2,y2):others
reducePolyline l = l

orderLinesDC :: [[[Polyline]]] -> [Polyline]
orderLinesDC segs =
	let
		halve l = splitAt (div (length l) 2) l
		splitOrder segs = case (\(x,y) -> (halve x, halve y)) $ unzip $ map (halve) segs of
			((a,b),(c,d)) -> orderLinesDC a ++ orderLinesDC b ++ orderLinesDC c ++ orderLinesDC d
	in
		if (length segs < 5 || length (head segs) < 5 ) then concat $ concat segs else
		case (\(x,y) -> (halve x, halve y)) $ unzip $ map (halve) segs of
			((a,b),(c,d)) ->orderLines $ 
				orderLinesDC a ++ orderLinesDC b ++ orderLinesDC c ++ orderLinesDC d

orderLinesP :: [[[Polyline]]] -> [Polyline]
orderLinesP segs =
	let
		halve l = splitAt (div (length l) 2) l
		splitOrder segs = case (\(x,y) -> (halve x, halve y)) $ unzip $ map (halve) segs of
			((a,b),(c,d)) -> orderLinesDC a ++ orderLinesDC b ++ orderLinesDC c ++ orderLinesDC d
		-- force is frome real world haskell
		force xs = go xs `pseq` ()
		    where go (_:xs) = go xs
		          go [] = 1
	in
		if (length segs < 5 || length (head segs) < 5 ) then concat $ concat segs else
		case (\(x,y) -> (halve x, halve y)) $ unzip $ map (halve) segs of
			((a,b),(c,d)) -> orderLines $ 
				let
					a' = orderLinesP a
					b' = orderLinesP b
					c' = orderLinesP c
					d' = orderLinesP d
				in (force a' `par` force b' `par` force c' `par` force d') `pseq` 
					(a' ++ b' ++ c' ++ d')


polylineNotNull (a:l) = not (null l)
polylineNotNull [] = False


getContour (x1,y1) (x2,y2) d obj = 
	let
		grid = [[getLineSeg (obj (x,-y), obj (x+d,-y), obj (x+d,-(y+d)), obj (x,-(y+d)), obj (x+d/2,-(y+d/2)) , (x-x1,y-y1), d ) | x <- [x1, x1+d.. x2]] | y <- [y1, y1 +d.. y2] ]
		multilines = (filter polylineNotNull) $ (map reducePolyline) $ orderLinesP grid
	in
		concat multilines

getMesh (x1,y1,z1) (x2,y2,z2) res obj = 
	let 
		dx = abs $ x2 - x1
		dy = abs $ y2 - y1
		dz = abs $ z2 - z1
		d = maximum [dx, dy, dz]
		ffloor = fromIntegral . floor
		fceil = fromIntegral . ceiling
	in
		if (abs.obj) ( (x1 + x2)/2, (y1 + y2)/2, (z1 + z2)/2) > d*0.9 then []
		else
		if d <= res
		then getTriangles ((obj(x1,y1,z1), obj(x1+res,y1,z1), obj(x1,y1+res,z1), obj(x1+res,y1+res,z1), obj(x1,y1,z1+res), obj(x1+res,y1,z1+res), obj(x1,y1+res,z1+res), obj(x1+res,y1+res,z1+res)), (x1,y1,z1), d )
		else let
			xs = if dx <= res then [(x1, x2)] else [(x1,xm), (xm, x2)] 
				where xm = x1 + res * fceil ( ffloor (dx/res) / 2.0)
			ys = if dy <= res then [(y1, y2)] else [(y1,xm), (xm, y2)] 
				where xm = y1 + res * fceil ( ffloor (dy/res) / 2.0)
			zs = if dz <= res then [(z1, z2)] else [(z1,xm), (xm, z2)] 
				where xm = z1 + res * fceil ( ffloor (dz/res) / 2.0)
			partitions = [getMesh (x1', y1', z1') (x2', y2', z2') res obj
				|  (x1',x2') <- xs, (y1', y2') <- ys, (z1',z2') <- zs ]
		in
			concat partitions


getMesh2 (x1,y1,z1) (x2,y2,z2) res obj = 
	let 
		dx = abs $ x2 - x1
		dy = abs $ y2 - y1
		dz = abs $ z2 - z1
		d = maximum [dx, dy, dz]
		ffloor = fromIntegral . floor
		fceil = fromIntegral . ceiling
	in
		if (abs.obj) ( (x1 + x2)/2, (y1 + y2)/2, (z1 + z2)/2) > d*0.9 then []
		else
		if d <= res
		then getTriangles ((obj(x1,y1,z1), obj(x1+res,y1,z1), obj(x1,y1+res,z1), obj(x1+res,y1+res,z1), obj(x1,y1,z1+res), obj(x1+res,y1,z1+res), obj(x1,y1+res,z1+res), obj(x1+res,y1+res,z1+res)), (x1,y1,z1), d )
		else let
			xs = if dx <= res then [(x1, x2)] else [(x1,xm), (xm, x2)] 
				where xm = x1 + res * fceil ( ffloor (dx/res) / 2.0)
			ys = if dy <= res then [(y1, y2)] else [(y1,xm), (xm, y2)] 
				where xm = y1 + res * fceil ( ffloor (dy/res) / 2.0)
			zs = if dz <= res then [(z1, z2)] else [(z1,xm), (xm, z2)] 
				where xm = z1 + res * fceil ( ffloor (dz/res) / 2.0)
			partitions = [getMesh (x1', y1', z1') (x2', y2', z2') res obj
				|  (x1',x2') <- xs, (y1', y2') <- ys, (z1',z2') <- zs ]
		in
			foldr1 par partitions `pseq` concat partitions


