{- ORMOLU_DISABLE -}
import Control.Applicative (pure)
import Graphics.Implicit
import Graphics.Implicit.Definitions
import Graphics.Implicit.Primitives

roundbox:: SymbolicObj3
roundbox =
  implicit
    (\(V3 x y z) -> x^4 + y^4 + z^4 - 15000)
    (pure (-20), pure 20)

main = writeSTL 2 "example16.stl" roundbox
