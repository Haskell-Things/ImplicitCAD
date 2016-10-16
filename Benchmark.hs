-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright (C) 2014 2015 2016, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

-- Benchmarks

-- Let's be explicit about where things come from :)

-- Use criterion for benchmarking. see <http://www.serpentine.com/criterion/>
import Criterion.Main

-- The parts of ImplicitCAD we know how to benchmark (in theory).
import Graphics.Implicit (union, circle, writeSVG, writePNG2, writePNG3, writeSTL, SymbolicObj2, SymbolicObj3)
import Graphics.Implicit.Export.SymbolicObj2 (symbolicGetContour)
import Graphics.Implicit.Export.SymbolicObj3 (symbolicGetMesh)
import Graphics.Implicit.Primitives (translate, difference, extrudeRM, rect3R)

-- Haskell representations of objects to benchmark.

--  FIXME: move each of these objects into seperate compilable files.

obj2d_1 :: SymbolicObj2
obj2d_1 =
    union
        [ circle 10
        , translate (22,0) $ circle 10
        , translate (0,22) $ circle 10
        , translate (-22,0) $ circle 10
        , translate (0,-22) $ circle 10
        ]

object1 :: SymbolicObj3
object1 = extrudeRM 0 (Just twist) Nothing Nothing obj2d_1 (Left 40)
    where twist h = 35*cos(h*2*pi/60)

object2 :: SymbolicObj3
object2 = squarePipe (10,10,10) 1 100
    where squarePipe (x,y,z) diameter precision =
            union
            $ map (\start-> translate start
                   $ rect3R 0 (0,0,0) (diameter,diameter,diameter)
                  )
            $ zip3 (map (\n->(n/precision)*x) [0..precision])
                   (map (\n->(n/precision)*y) [0..precision])
                   (map (\n->(n/precision)*z) [0..precision])

object3 :: SymbolicObj3
object3 =
    difference
        [ rect3R 1 (-1,-1,-1) (1,1,1)
        , rect3R 1 (0,0,0) (2,2,2)
        ]

obj2Benchmarks :: String -> SymbolicObj2 -> Benchmark
obj2Benchmarks name obj =
    bgroup name
    [
--        bench "SVG write" $ writeSVG 1 "benchmark.svg" obj
--      , bench "PNG write" $ writePNG2 1 "benchmark.png" obj
--      ,
          bench "Get contour" $ nf (symbolicGetContour 1) obj
    ]

obj3Benchmarks :: String -> SymbolicObj3 -> Benchmark
obj3Benchmarks name obj =
    bgroup name
    [
--        bench "PNG write" $ writePNG3 1 "benchmark.png" obj
--      , bench "STL write" $ writeSTL 1 "benchmark.stl" obj
--      ,
      bench "Get mesh" $ nf (symbolicGetMesh 1) obj
    ]

benchmarks =
    [ obj3Benchmarks "Object 1" object1
    , obj3Benchmarks "Object 2" object2
    , obj3Benchmarks "Object 3" object3
    ]

main = defaultMain benchmarks

