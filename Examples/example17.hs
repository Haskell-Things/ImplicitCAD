-- Example 17, pulled from our benchmarking suite.
import Graphics.Implicit
import Graphics.Implicit.Definitions

default (Fastℕ, ℝ)

object2 :: SymbolicObj3
object2 = squarePipe (10,10,10) 1 100
    where
      squarePipe :: (ℝ,ℝ,ℝ) -> ℝ -> ℝ -> SymbolicObj3
      squarePipe (x,y,z) diameter precision =
            union
            $ map (\start-> translate start
                   $ rect3R 0 (0,0,0) (diameter,diameter,diameter)
                  )
            $ zip3 (map (\n->(fromIntegral n/precision)*x) [0..100])
                   (map (\n->(fromIntegral n/precision)*y) [0..100])
                   (map (\n->(fromIntegral n/precision)*z) [0..100])

main = writeSTL 1 "example17.stl" object2

