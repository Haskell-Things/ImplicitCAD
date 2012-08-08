-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

module Graphics.Implicit.Export.Render.HandleSquares (mergedSquareTris) where

import Graphics.Implicit.Definitions
import Graphics.Implicit.Export.Render.Definitions
import qualified Graphics.Implicit.SaneOperators as S
import GHC.Exts (groupWith)

mergedSquareTris sqTris = 
	let
		triTriangles = concat $ map (\(Tris a) -> a) $ filter isTris sqTris	
		squares = filter (not . isTris) sqTris
		planeAligned = groupWith (\(Sq basis z _ _) -> (basis,z)) squares
		joined = map 
			( concat . (map joinYaligned) . groupWith (\(Sq _ _ _ yS) -> yS)
			. concat . (map joinXaligned) . groupWith (\(Sq _ _ xS _) -> xS)) 
			planeAligned
		finishedSquares = concat joined
	in
		triTriangles ++ concat (map squareToTri finishedSquares)


isTris (Tris _) = True
isTris _ = False

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


squareToTri (Sq (b1,b2,b3) z (x1,x2) (y1,y2)) =
	let
		zV = b3 S.* z
		(x1V, x2V) = (x1 S.* b1, x2 S.* b1)
		(y1V, y2V) = (y1 S.* b2, y2 S.* b2)
		a = zV S.+ x1V S.+ y1V
		b = zV S.+ x2V S.+ y1V
		c = zV S.+ x1V S.+ y2V
		d = zV S.+ x2V S.+ y2V
	in
		[(a,b,c),(c,b,d)]


