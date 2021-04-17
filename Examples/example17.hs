-- Example 17, pulled from our benchmarking suite.
import Control.Applicative (pure)
import Prelude ((<$>), ($), zipWith3, fmap, fromIntegral, (*), (/), Bool(..))
import Graphics.Implicit (cube, union, translate, writeSTL, V3(..))
import Graphics.Implicit.Definitions (Fastℕ, ℝ, ℝ3, SymbolicObj3)

default (Fastℕ, ℝ)

object2 :: SymbolicObj3
object2 = squarePipe (pure 10) 1 100
    where
      squarePipe :: ℝ3 -> ℝ -> ℝ -> SymbolicObj3
      squarePipe (V3 x y z) diameter precision =
            union
            ((\start -> translate start
               $ cube True (pure diameter)
             )
             <$>
              zipWith3
                V3
                (fmap (\n -> (fromIntegral n / precision) * x) [0..100])
                (fmap (\n -> (fromIntegral n / precision) * y) [0..100])
                (fmap (\n -> (fromIntegral n / precision) * z) [0..100]))

main = writeSTL 1 "example17.stl" object2
