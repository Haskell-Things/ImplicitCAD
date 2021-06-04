{- ORMOLU_DISABLE -}
-- Example 13 - the rounded union of a cube and a sphere.
import Control.Applicative (pure)
import Graphics.Implicit

out = union [
    cube False (pure 20) -- same as (V3 20 20 20)
  , translate (pure 20) $ sphere 15
  ]

main = writeSTL 1 "example13.stl" out
