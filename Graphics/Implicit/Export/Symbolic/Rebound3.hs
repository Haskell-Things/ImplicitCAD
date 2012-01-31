module Graphics.Implicit.Export.Symbolic.Rebound3 (rebound3) where

import Graphics.Implicit.Definitions
import qualified Graphics.Implicit.SaneOperators as S

rebound3 :: BoxedObj3 -> BoxedObj3
rebound3 (obj, (a,b)) = 
	let
		d :: ℝ3
		d = (b S.- a) S./ (10.0 :: ℝ)
	in 
		(obj, ((a S.- d), (b S.+ d)))

