module Graphics.Implicit.Export.Symbolic.Rebound2 (rebound2) where

import Graphics.Implicit.Definitions
import qualified Graphics.Implicit.SaneOperators as S

rebound2 :: BoxedObj2 -> BoxedObj2
rebound2 (obj, (a,b)) = 
	let
		d :: ℝ2
		d = (b S.- a) S./ (10.0 :: ℝ)
	in 
		(obj, ((a S.- d), (b S.+ d)))
