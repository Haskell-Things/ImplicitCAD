import Graphics.Implicit
import Graphics.Implicit.Definitions
import Graphics.Implicit.Primitives

roundbox:: SymbolicObj3
roundbox = implicit (\(x,y,z) -> x^4 + y^4 + z^4 - 15000) ((-20,-20,-20),(20,20,20))

main = writeSTL 2 "example16.stl" roundbox
