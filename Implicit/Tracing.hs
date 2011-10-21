-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

module Implicit.Tracing (
	getTriangles,
	getLineSeg,
	orderLines,
	orderLinesDC,
	orderLinesP,
	reducePolyline,
	polylineNotNull
) where

import Implicit.Definitions
import Implicit.Tracing.GetTriangles (getTriangles)

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

