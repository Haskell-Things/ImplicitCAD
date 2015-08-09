module Graphics.Implicit.Export.Symbolic.Rebound3 (rebound3) where

import Graphics.Implicit.Definitions
import Data.VectorSpace

rebound3 :: BoxedObj3 -> BoxedObj3
rebound3 (obj, (a,b)) = 
    let
        d :: ℝ3
        d = (b ^-^ a) ^/ 10
    in 
        (obj, ((a ^-^ d), (b ^+^ d)))

