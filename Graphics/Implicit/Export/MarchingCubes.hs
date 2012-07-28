-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

module Graphics.Implicit.Export.MarchingCubes (getMesh, getMesh') where

import Graphics.Implicit.Definitions
import qualified Graphics.Implicit.SaneOperators as S
import Control.Parallel.Strategies (using, rdeepseq, parListChunk)
import Debug.Trace


getMesh = getMesh'

getMesh' :: ℝ3 -> ℝ3 -> ℝ -> Obj3 -> TriangleMesh
getMesh' (x1, y1, z1) (x2, y2, z2) res obj = 
	let
		dx = x2-x1
		dy = y2-y1
		dz = z2-z1
		-- How many steps will we take on each axis?
		nx = fromIntegral $ ceiling $ dx / res
		ny = fromIntegral $ ceiling $ dy / res
		nz = fromIntegral $ ceiling $ dz / res
		tris = [
		           getTriangles 
		           (x1+dx*mx/nx,y1+dy*my/ny,z1+dz*mz/nz) 
		           (x1+dx*(mx+1)/nx,y1+dy*(my+1)/ny,z1+dz*(mz+1)/nz)
		           obj		       | mx <- [0..nx-1], my <- [0..ny-1], mz <- [0..nz-1] 
		       ] `using` (parListChunk (floor nx* floor ny*(max 1 $ div (floor nz) 32)) rdeepseq)
	in concat tris


getTriangles :: ℝ3 -> ℝ3 -> Obj3 -> [Triangle]
{-- INLINE getTriangles #-}
getTriangles (x1,y1,z1) (x2,y2,z2) obj =
	let
		res = x2 - x1

		inj1 a (b,c) = (a,b,c)
		inj2 b (a,c) = (a,b,c)
		inj3 c (a,b) = (a,b,c)

		infixr 0 $**
		infixr 0 *$*
		infixr 0 **$
		f $** a = \(b,c) -> f (a,b,c)
		f *$* b = \(a,c) -> f (a,b,c)
		f **$ c = \(a,b) -> f (a,b,c)

		map2 f = map (map f)
		map2R f = map (reverse . map f)

		segs = concat $ [
				map2  (inj3 z1) $ getSegs (x1,y1) (x2,y2) (obj **$ z1),
				map2R (inj3 z2) $ getSegs (x1,y1) (x2,y2) (obj **$ z2),
				map2R (inj2 y1) $ getSegs (x1,z1) (x2,z2) (obj *$* y1),
				map2  (inj2 y2) $ getSegs (x1,z1) (x2,z2) (obj *$* y2),
				map2  (inj1 x1) $ getSegs (y1,z1) (y2,z2) (obj $** x1),
				map2R (inj1 x2) $ getSegs (y1,z1) (y2,z2) (obj $** x2)
			]
		loops = getLoops segs

		fillLoop3 :: [ ℝ3 ] -> [(ℝ3, ℝ3, ℝ3)]
		{-- INLINE fillLoop3 #-}
		fillLoop3 []      = []
		fillLoop3 [a]     = []
		fillLoop3 [a,b]   = []
		fillLoop3 [a,b,c] = [(a,b,c)]
		fillLoop3 path    =
			let
				len = fromIntegral $ length path :: ℝ
				mid@(midx,midy,midz) = (foldl1 (S.+) path) S./ len
				midval = obj mid
				normal = S.normalized $ foldl1 (S.+) $
					[ a S.⨯ b | (a,b) <- zip path (tail path ++ [head path]) ]
				deriv = (obj (mid S.+ normal S.* (res/100) ) - midval)/res*100
			in if abs midval < res/100 || abs deriv < 0.5
				then case path of 
					[a@(a1,a2,a3),b,c@(c1,c2,c3),d] -> 
						if obj ((a1+c1)/2,(a2+c2)/2,(a3+c3)/2) < res/100
							then [(a,b,c), (a,c,d)]
							else [(a,b,mid) | (a,b) <- zip path (tail path ++ [head path]) ]
					_ -> [(a,b,mid) | (a,b) <- zip path (tail path ++ [head path]) ]
				else let
					mid' = mid S.- normal S.* (midval/deriv)
				in [(a,b,mid') | (a,b) <- zip path (tail path ++ [head path]) ]

		tris = concat $ map fillLoop3 loops
	in tris

getLoops :: (Show a, Eq a) => [[a]] -> [[a]]
{-- INLINE getLoops #-}
getLoops [] = []
getLoops (loop:loops) | head loop == last loop =
	init loop : getLoops loops
getLoops (loop:loops) | head loop /= last loop && traceShow (loop:loops) True =
	let
		loopEnd = last loop
		connectsToLoop (x:xs) = x == loopEnd
		possibleConts = filter connectsToLoop loops
		nonConts = filter (not . connectsToLoop) loops
		(newLoop, unused) = if null possibleConts
			then trace ("unclosed loop in paths given: \n" ++ show (loop:loops) 
				      ++"\nnothing attaches to " ++ show loopEnd) ([], nonConts)
			else (loop ++ tail (head possibleConts), tail possibleConts ++ nonConts)
	in
		getLoops ( if null newLoop then unused else newLoop:unused)


getSegs :: ℝ2 -> ℝ2 -> Obj2 -> [Polyline]
{-- INLINE getSegs #-}
getSegs (x1, y1) (x2, y2) obj = 
	let 
		(x,y) = (x1, y1)

		-- Let's evlauate obj at a few points...
		x1y1 = obj (x1, y1)
		x2y1 = obj (x2, y1)
		x1y2 = obj (x1, y2)
		x2y2 = obj (x2, y2)
		c = obj ((x1+x2)/2, (y1+y2)/2)

		dx = x2 - x1
		dy = y2 - y1
		res = sqrt (dx*dy)

		-- linearly interpolated midpoints on the relevant axis

		midx1 = (x,                       y + dy*x1y1/(x1y1-x1y2))
		midx2 = (x + dx,                  y + dy*x2y1/(x2y1-x2y2))
		midy1 = (x + dx*x1y1/(x1y1-x2y1), y )
		midy2 = (x + dx*x1y2/(x1y2-x2y2), y + dy)

		notPointLine (p1:p2:[]) = p1 /= p2

		detail [p1@(x1,y1), p2@(x2,y2)] =
			let
				mid@(midx, midy) = (p1 S.+ p2) S./ (2 :: ℝ)
				midval = obj mid 
			in if traceShow (mid, midval) $ abs midval < res / 100
				then [(x1,y1), (x2,y2)]
				else let
					normal = (\(a,b) -> (b,-a) ) $ S.normalized (p2 S.- p1)
					deriv = (obj (mid S.+ normal S.* (res/100)) - midval) S.* (100/res)
				in if deriv == 0 || abs (midval / deriv) > res || abs (midval / deriv) < res/100
					then [(x1,y1), (x2,y2)]
					else [(x1,y1), (mid S.- normal S.* (midval/deriv)) , (x2,y2)]
		detail x = x

	in map detail $ filter (notPointLine) $ case (x1y2 <= 0, x2y2 <= 0,
	                                 x1y1 <= 0, x2y1 <= 0) of

		-- Yes, there's some symetries that could reduce the amount of code...
		-- But I don't think they're worth exploiting...
		(True,  True, 
		 True,  True)  -> []
		(False, False,
		 False, False) -> []
		(True,  True, 
		 False, False) -> [[midx1, midx2]]
		(False, False,
		 True,  True)  -> [[midx2, midx1]]
		(False, True, 
		 False, True)  -> [[midy2, midy1]]
		(True,  False,
		 True,  False) -> [[midy1, midy2]]
		(True,  False,
		 False, False) -> [[midx1, midy2]]
		(False, True, 
		 True,  True)  -> [[midy2, midx1]]
		(True,  True, 
		 False, True)  -> [[midx1, midy1]]
		(False, False,
		 True,  False) -> [[midy1, midx1]]
		(True,  True, 
		 True,  False) -> [[midy1, midx2]]
		(False, False,
		 False, True)  -> [[midx2, midy1]]
		(True,  False,
		 True,  True)  -> [[midx2, midy2]]
		(False, True, 
		 False, False) -> [[midy2, midx2]]
		(True,  False,
		 False, True)  -> if c > 0
			then [[midx1, midy2], [midx2, midy1]]
			else [[midx1, midy1], [midx2, midy2]]
		(False, True, 
		 True,  False) -> if c <= 0
			then [[midx1, midy2], [midx2, midy1]]
			else [[midx1, midy1], [midx2, midy2]]

