-- Example 13 - the rounded union of a cube and a sphere.
import Graphics.Implicit

out = union [
                rect3R 0 (0,0,0) (20,20,20),
                translate (20,20,20) (sphere 15) ]

main = writeSTL 1 "example13.stl" out
