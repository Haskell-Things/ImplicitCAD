-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

module Graphics.Implicit.Export.Render.TesselateLoops (tesselateLoop) where

import Graphics.Implicit.Definitions
import Graphics.Implicit.Export.Render.Definitions
import qualified Graphics.Implicit.SaneOperators as S
import Graphics.Implicit.SaneOperators ((⋅),norm,(⨯),normalized)

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

tesselateLoop res obj [[a,_],[b,_],[c,_],[d,_]] | (a S.+ c) == (b S.+ d) =
	let
		b1 = normalized $ a S.- b
		b2 = normalized $ c S.- b
		b3 = b1 ⨯ b2
	in if norm b3 == 1
	then [Sq (b1,b2,b3) (a ⋅ b3) (a ⋅ b1, c ⋅ b1) (a ⋅ b2, c ⋅ b2) ]
	else return $ Tris $ [(a,b,c),(a,c,d)]

tesselateLoop res obj [[a,_],[b,_],[c,_],[d,_]] | obj ((a S.+ c) S./ (2 :: ℝ)) < res/30 =
	return $ Tris $ [(a,b,c),(a,c,d)]

tesselateLoop res obj pathSides = return $ Tris $
	let
		path = concat $ map init pathSides
		len = fromIntegral $ length path :: ℝ
		mid@(midx,midy,midz) = (foldl1 (S.+) path) S./ len
		midval = obj mid
		preNormal = foldl1 (S.+) $
			[ a ⨯ b | (a,b) <- zip path (tail path ++ [head path]) ]
		preNormalNorm = norm preNormal
		normal = preNormal S./ preNormalNorm
		deriv = (obj (mid S.+ (normal S.* (res/100)) ) - midval)/res*100
		mid' = mid S.- normal S.* (midval/deriv)
	in if abs midval > res/50 && preNormalNorm > 0.5 && abs deriv > 0.5 
		      && abs (deriv*midval) < 1.1*res && 5*abs (obj mid') < abs midval
		then [(a,b,mid') | (a,b) <- zip path (tail path ++ [head path]) ]
		else [(a,b,mid) | (a,b) <- zip path (tail path ++ [head path]) ]

