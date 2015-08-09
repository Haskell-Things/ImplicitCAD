module Graphics.Implicit.Export.Symbolic.Rebound2 (rebound2) where

import Data.VectorSpace
import Graphics.Implicit.Definitions

rebound2 :: BoxedObj2 -> BoxedObj2
rebound2 (obj, (a,b)) = 
    let
        d :: ℝ2
        d = (b ^-^ a) ^/ 10
    in 
        (obj, ((a ^-^ d), (b ^+^ d)))
