-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

{-# LANGUAGE NoMonomorphismRestriction, ViewPatterns #-}

module Graphics.Implicit.Export.MarchingCubes (getMesh, getMesh') where

import Graphics.Implicit.Definitions
import qualified Graphics.Implicit.SaneOperators as S
import Control.Parallel.Strategies (using, rdeepseq, parListChunk)
import GHC.Exts (groupWith)
import Control.DeepSeq (NFData, rnf)
import Debug.Trace
trace2 a = traceShow a a

(⋅)  = (S.⋅)
(⨯)  = (S.⨯)
(^+) = (S.+)
(^-) = (S.-)
(^*) = (S.*)
(^/) = (S./)
norm = S.norm
normalized = S.normalized

mid (x:xs) = init xs

data TriSquare = Sq (ℝ3,ℝ3,ℝ3) ℝ ℝ2 ℝ2 | Tris [Triangle]

instance NFData TriSquare where
	rnf (Sq b z xS yS) = rnf (b,z,xS,yS)
	rnf (Tris tris) = rnf tris


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

		inj1 a (b,c) = (a,b,c)
		inj2 b (a,c) = (a,b,c)
		inj3 c (a,b) = (a,b,c)

		infixr 0 $**
		infixr 0 *$*
		infixr 0 **$
		f $** a = \(b,c) -> f (a,b,c)
		f *$* b = \(a,c) -> f (a,b,c)
		f **$ c = \(a,b) -> f (a,b,c)

		infixr 0 $$*
		infixr 0 *$$
		infixr 0 $*$
		(f $$* a) b = \c -> f (a,b,c)
		(f *$$ b) c = \a -> f (a,b,c)
		(f $*$ a) c = \b -> f (a,b,c)

		map2 f = map (map f)
		map2R f = map (reverse . map f)
		mapR = map reverse

		midsZ = [[[let obj' = (obj $$* (x1+dx*mx/nx)) (y1+dy*my/ny) in
		          interpolate 0 
		              (z1+dz*mz/nz, obj' (z1+dz*mz/nz)) 
		              (z1+dz*(mz+1)/nz, obj' (z1+dz*(mz+1)/nz))
		              obj' res
		       | mx <- [0..nx + 1] ] | my <- [0..ny + 1] ] | mz <- [0..nz+1]
		       ] `using` (parListChunk (max 1 $ div (floor nz) 32) rdeepseq)

		midsY =[[[let obj' = (obj $*$ (x1+dx*mx/nx) ) (z1+dz*mz/nz) in 
		           interpolate 0 
		              (y1+dy*my/ny, obj' (y1+dy*my/ny)) 
		              (y1+dy*(my+1)/ny, obj' (y1+dy*(my+1)/ny))
		              obj' res
		       | mx <- [0..nx+1] ] | my <- [0..ny+1] ] | mz <- [0..nz+1]
		       ] `using` (parListChunk (max 1 $ div (floor nz) 32) rdeepseq)

		midsX = [[[let obj' = (obj *$$ (y1+dy*my/ny) ) (z1+dz*mz/nz) in 
		           interpolate 0 
		              (x1+dx*mx/nx, obj' (x1+dx*mx/nx)) 
		              (x1+dx*(mx+1)/nx, obj' (x1+dx*(mx+1)/nx))
		              obj' res
		       | mx <- [0..nx+1] ] | my <- [0..ny+1] ] | mz <- [0..nz+1]
		       ] `using` (parListChunk (max 1 $ div (floor nz) 32) rdeepseq)

		segsZ =[[[let (mx',my',mz') = (floor mx, floor my, floor mz) in
		       map2  (inj3 (z1+dz*mz/nz)) $ getSegs 
		           (x1+dx*mx/nx,    y1+dy*my/ny)
                   (x1+dx*(mx+1)/nx,y1+dy*(my+1)/ny)
                   (obj **$ (z1+dz*mz/nz))
		           (midsY !! mz' !! my' !! mx', midsY !! mz' !! my' !! (mx'+1),
		            midsX !! mz' !! my' !! mx', midsX !! mz' !! (my'+1) !! mx')

		       | mx <- [0..nx  ] ] | my <- [0..ny-1] ] | mz <- [0..nz  ]
		       ] `using` (parListChunk (max 1 $ div (floor nz) 32) rdeepseq)

		segsY = [[[let (mx',my',mz') = (floor mx, floor my, floor mz) in
		       map2  (inj2 (y1+dy*my/ny)) $ getSegs 
		           (x1+dx*mx/nx,    z1+dz*mz/nz)
                   (x1+dx*(mx+1)/nx,z1+dz*(mz+1)/nz)
                   (obj *$* (y1+dy*my/ny))
		           (midsZ !! mz' !! my' !! mx', midsZ !! mz' !! my' !! (mx'+1),
		            midsX !! mz' !! my' !! mx', midsX !! (mz'+1) !! my' !! mx')
		       | mx <- [0..nx-1] ] | my <- [0..ny  ] ] | mz <- [0..nz-1]
		       ] `using` (parListChunk (max 1 $ div (floor nz) 32) rdeepseq)

		segsX =[[[let (mx',my',mz') = (floor mx, floor my, floor mz) in
		       map2  (inj1 (x1+dx*mx/nx)) $ getSegs 
		           (y1+dy*my/ny,    z1+dz*mz/nz)
                   (y1+dy*(my+1)/ny,z1+dz*(mz+1)/nz)
                   (obj $** (x1+dx*mx/nx))
		           (midsZ !! mz' !! my' !! mx', midsZ !! mz' !! (my'+1) !! mx',
		            midsY !! mz' !! my' !! mx', midsY !! (mz'+1) !! my' !! mx')
		       | mx <- [0..nx  ] ] | my <- [0..ny-1] ] | mz <- [0..nz-1]
		       ] `using` (parListChunk (max 1 $ div (floor nz) 32) rdeepseq)
 
		--tris :: [ [(ℝ3,ℝ3,ℝ3)] ]
		sqTris = [
		        concat $ map (tesselateLoop res obj) $ getLoops2' $ concat [
		                     segsX !! mz     !! my     !! mx,
		              mapR $ segsX !! mz     !! my     !!(mx + 1),
		              mapR $ segsY !! mz     !! my     !! mx,
		                     segsY !! mz     !!(my + 1)!! mx,
		                     segsZ !! mz     !! my     !! mx,
		              mapR $ segsZ !!(mz + 1)!! my     !! mx
		          ]
		       | mx <- [0.. floor nx-1], my <- [0.. floor ny-1], mz <- [0..floor nz-1]
		       ] `using` (parListChunk (floor nx* floor ny*(max 1 $ div (floor nz) 32)) rdeepseq)
	
	in genTris $ concat sqTris

interpolate _ (a, aval) (b, bval) _ _ | aval*bval > 0 = a -- error $ "interval " ++ show (a,b) ++ " doesn't necessarily cross 0 -- values " ++ show (aval, bval)
interpolate _ (a, 0) _ _ _ = a
interpolate _ _ (b, 0) _ _ = b
interpolate n (a, aval) (b, bval) obj res | aval /= bval= 
	let
		mid = a + (b-a)*aval/(aval-bval)
		midval = obj mid
	in if abs midval < res/300 || mid > 2
	then mid
	else if midval * aval > 0
	then interpolate (n+1) (mid, midval) (b, bval) obj res
	else interpolate (n+1) (a,aval) (mid, midval) obj res
interpolate _ (a, _) _ _ _ = a

genTris sqTris = 
	let
		isTris (Tris _) = True
		isTris _ = False
		triTriangles = concat $ map (\(Tris a) -> a) $ filter isTris sqTris	
		squares = filter (not . isTris) sqTris
		planeAligned = groupWith (\(Sq basis z _ _) -> (basis,z)) squares
		joinXaligned (pres@(Sq b z xS (y1,y2)):sqs) = 
			let
				isNext (Sq _ _ _ (a,_)) = a == y2
				isPrev (Sq _ _ _ (_,a)) = a == y1
			in case filter isNext sqs of
				[Sq _ _ _ (_, y3)] -> 
					joinXaligned ((Sq b z xS (y1,y3)):(filter (not.isNext) sqs))
				_ -> case filter isPrev sqs of
					[Sq _ _ _ (y0, _)] -> 
						joinXaligned ((Sq b z xS (y0,y2)):(filter (not.isPrev) sqs))
					_ -> pres : joinXaligned sqs
		joinXaligned [] = []
		joinYaligned (pres@(Sq b z (x1,x2) yS):sqs) = 
			let
				isNext (Sq _ _ (a,_) _) = a == x2
				isPrev (Sq _ _ (_,a) _) = a == x1
			in case filter isNext sqs of
				[Sq _ _ (_, x3) _] -> 
					joinYaligned ((Sq b z (x1,x3) yS):(filter (not.isNext) sqs))
				_ -> case filter isPrev sqs of
					[Sq _ _ (x0, _) _] -> 
						joinYaligned ((Sq b z (x0,x2) yS):(filter (not.isPrev) sqs))
					_ -> pres : joinYaligned sqs
		joinYaligned [] = []
		joined = map 
			( concat . (map joinYaligned) . groupWith (\(Sq _ _ _ yS) -> yS)
			. concat . (map joinXaligned) . groupWith (\(Sq _ _ xS _) -> xS)) 
			planeAligned
		finishedSquares = concat joined
		squareToTri (Sq (b1,b2,b3) z (x1,x2) (y1,y2)) =
			let
				zV = b3 ^* z
				(x1V, x2V) = (x1 ^* b1, x2 ^* b1)
				(y1V, y2V) = (y1 ^* b2, y2 ^* b2)
				a = zV ^+ x1V ^+ y1V
				b = zV ^+ x2V ^+ y1V
				c = zV ^+ x1V ^+ y2V
				d = zV ^+ x2V ^+ y2V
			in
				[(a,b,c),(c,b,d)]
	in
		triTriangles ++ concat (map squareToTri finishedSquares)
				

tesselateLoop :: ℝ -> Obj3 -> [[ℝ3]] -> [TriSquare]

tesselateLoop _ _ [] = []

tesselateLoop _ _ [[a,b],[_,c],[_,_]] = return $ Tris [(a,b,c)]

tesselateLoop res obj [[_,_], as@(_:_:_:_),[_,_], bs@(_:_:_:_)] | length as == length bs =
	concat $ map (tesselateLoop res obj) $ 
		[[[a1,b1],[b1,b2],[b2,a2],[a2,a1]] | ((a1,b1),(a2,b2)) <- zip (init pairs) (tail pairs)]
			where pairs = zip (reverse as) bs

tesselateLoop res obj [as@(_:_:_:_),[_,_], bs@(_:_:_:_), [_,_] ] | length as == length bs =
	concat $ map (tesselateLoop res obj) $ 
		[[[a1,b1],[b1,b2],[b2,a2],[a2,a1]] | ((a1,b1),(a2,b2)) <- zip (init pairs) (tail pairs)]
			where pairs = zip (reverse as) bs

tesselateLoop res obj [[a,_],[b,_],[c,_],[d,_]] | (a ^+ c) == (b ^+ d) =
	let
		b1 = normalized $ a ^- b
		b2 = normalized $ c ^- b
		b3 = b1 ⨯ b2
	in if norm b3 == 1
	then [Sq (b1,b2,b3) (a ⋅ b3) (a ⋅ b1, c ⋅ b1) (a ⋅ b2, c ⋅ b2) ]
	else return $ Tris $ [(a,b,c),(a,c,d)]

tesselateLoop res obj [[a,_],[b,_],[c,_],[d,_]] | obj ((a ^+ c) ^/ (2 :: ℝ)) < res/30 =
	return $ Tris $ [(a,b,c),(a,c,d)]

tesselateLoop res obj pathSides = return $ Tris $
	let
		path = concat $ map init pathSides
		len = fromIntegral $ length path :: ℝ
		mid@(midx,midy,midz) = (foldl1 (S.+) path) S./ len
		midval = obj mid
		preNormal = foldl1 (S.+) $
			[ a S.⨯ b | (a,b) <- zip path (tail path ++ [head path]) ]
		preNormalNorm = norm preNormal
		normal = preNormal ^/ preNormalNorm
		deriv = (obj (mid S.+ normal S.* (res/100) ) - midval)/res*100
		mid' = mid S.- normal S.* (midval/deriv)
	in if abs midval > res/50 && preNormalNorm > 0.5 && abs deriv > 0.5
	               && 5*abs (obj mid') < abs midval
		then [(a,b,mid') | (a,b) <- zip path (tail path ++ [head path]) ]
		else [(a,b,mid) | (a,b) <- zip path (tail path ++ [head path]) ]

getLoops2' a = getLoops2 a []

getLoops2 :: (Show a, Eq a) => [[a]] -> [[a]] -> [[[a]]]
{-- INLINE getLoops2 #-}
getLoops2 [] [] = []
getLoops2 (x:xs) [] = getLoops2 xs [x]
getLoops2 segs workingLoop | head (head workingLoop) == last (last workingLoop) =
	workingLoop : getLoops2 segs []
getLoops2 segs workingLoop =
	let
		presEnd = last $ last workingLoop
		connects (x:xs) = x == presEnd
		possibleConts = filter connects segs
		nonConts = filter (not . connects) segs
		(next, unused) = if null possibleConts
			then trace ("unclosed loop in paths given\n" 
			          ++ show segs ++ "\n" ++ show workingLoop) ([], nonConts)
			else (head possibleConts, tail possibleConts ++ nonConts)
	in
		if null next
		then workingLoop : getLoops2 segs []
		else getLoops2 unused (workingLoop ++ [next])
		


detail' res obj [p1@(x1,y1), p2@(x2,y2)] | (x2-x1)^2 + (y2-y1)^2 > res^2/25 = 
		detail 0 res obj [p1,p2]
detail' _ _ a = a

detail :: Int -> ℝ -> (ℝ2 -> ℝ) -> [ℝ2] -> [ℝ2]
detail n res obj [p1@(x1,y1), p2@(x2,y2)] | n < 2 =
	let
		mid@(midX, midY) = (p1 S.+ p2) S./ (2 :: ℝ)
		midval = obj mid 
	in if abs midval < res / 30
	then [(x1,y1), (x2,y2)]
	else let
		normal = (\(a,b) -> (b, -a)) $ normalized (p2 ^- p1) 
		derivN = -(obj (mid ^- (normal ^* (midval/2))) - midval) ^* (2/midval)
	in if abs derivN > 0.3 && abs derivN < 2
	then let
		mid' = mid ^- (normal ^* (midval / derivN))
	in detail (n+1) res obj [(x1,y1), mid'] 
	   ++ tail (detail (n+1) res obj [mid', (x2,y2)] )
	else let
		derivX = (obj (midX + res/100, midY) - midval)*100/res
		derivY = (obj (midX, midY + res/100) - midval)*100/res
		derivNormSq = derivX^2+derivY^2
	in if abs derivNormSq > 0.09 && abs derivNormSq < 4
	then let
		(dX, dY) = (- derivX*midval/derivNormSq, - derivY*midval/derivNormSq)
		mid'@(midX', midY') = 
			(midX + dX, midY + dY)
		midval' = obj mid'
		posRatio = midval/(midval - midval')
		mid''@(midX'', midY'') = (midX + dX*posRatio, midY + dY*posRatio)
	in 
		detail (n+1) res obj [(x1,y1), mid''] ++ tail (detail (n+1) res obj [mid'', (x2,y2)] )
	else [(x1,y1), (x2,y2)]


detail _ _ _ x = x

simplify res = {-simplify3 . simplify2 res . -} simplify1

simplify1 :: [ℝ2] -> [ℝ2]
simplify1 (a:b:c:xs) =
	if abs ( ((b ^- a) ⋅ (c ^- a)) - norm (b ^- a) * norm (c ^- a) ) < 0.0001
	then simplify1 (a:c:xs)
	else a : simplify1 (b:c:xs)
simplify1 a = a

simplify2 :: ℝ -> [ℝ2] -> [ℝ2]
simplify2 res [a,b,c,d] = 
	if norm (b ^- c) < res/10
	then [a, ((b ^+ c) ^/ (2::ℝ)), d]
	else [a,b,c,d]
simplify2 _ a = a

simplify3 (a:as) | length as > 5 = simplify3 $ a : half (init as) ++ [last as]
	where
		half (a:b:xs) = a : half xs
		half a = a
simplify3 a = a

getSegs :: ℝ2 -> ℝ2 -> Obj2 -> (ℝ,ℝ,ℝ,ℝ) -> [Polyline]
{-- INLINE getSegs #-}
getSegs (x1, y1) (x2, y2) obj (midx1V,midx2V,midy1V,midy2V) = 
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

		midx1 = (x,      midx1V )
		midx2 = (x + dx, midx2V )
		midy1 = (midy1V , y )
		midy2 = (midy2V, y + dy)

		notPointLine (p1:p2:[]) = p1 /= p2

	in map (simplify res . detail' res obj) $ filter (notPointLine) $ case (x1y2 <= 0, x2y2 <= 0,
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
		 False, True)  -> if c <= 0
			then [[midx1, midy1], [midx2, midy2]]
			else [[midx1, midy2], [midx2, midy1]]
		(False, True, 
		 True,  False) -> if c <= 0
			then [[midy2, midx1], [midy1, midx2]]
			else [[midy1, midx1], [midy2, midx2]]

