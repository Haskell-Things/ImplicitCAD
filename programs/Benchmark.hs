{- ORMOLU_DISABLE -}
-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright (C) 2014 2015 2016, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

-- Our benchmarking suite.

-- Let's be explicit about where things come from :)

import Prelude (pure, ($), (*), (/), String, IO, cos, pi, fmap, zip3, Either(Left, Right), fromIntegral, (<>), (<$>))

-- Use criterion for benchmarking. see <http://www.serpentine.com/criterion/>
import Criterion.Main (Benchmark, bgroup, bench, nf, nfAppIO, defaultMain)

-- The parts of ImplicitCAD we know how to benchmark.
import Graphics.Implicit (union, circle, sphere, SymbolicObj2, SymbolicObj3, ExtrudeMScale(C1), writeDXF2, writeSVG, writePNG2, writeSTL, writeBinSTL, unionR, translate, difference, extrudeM, rect3, withRounding)
-- The default object context and variables defining distance and counting in our world.
import Graphics.Implicit.Definitions (defaultObjectContext, ℝ, Fastℕ)
import Graphics.Implicit.Export.SymbolicObj2 (symbolicGetContour)
import Graphics.Implicit.Export.SymbolicObj3 (symbolicGetMesh)

-- Vectors.
import Linear(V2(V2), V3(V3))

-- Haskell representations of objects to benchmark.

-- FIXME: move each of these objects into seperate compilable files.

-- | What we extrude in the example on the website.
obj2d_1 :: SymbolicObj2
obj2d_1 =
    unionR 8
        [ circle 10
        , translate (V2 22 0) $ circle 10
        , translate (V2 0 22) $ circle 10
        , translate (V2 (-22) 0) $ circle 10
        , translate (V2 0 (-22)) $ circle 10
        ]

-- | An extruded version of obj2d_1, should be identical to the website's example, and example5.escad.
object1 :: SymbolicObj3
object1 = extrudeM (Right twist) (C1 1) (Left (V2 0 0)) obj2d_1 (Left 40)
    where
      twist :: ℝ -> ℝ
      twist h = 35*cos(h*2*pi/60)

-- | another 3D object, for benchmarking.
object2 :: SymbolicObj3
object2 = squarePipe (10,10,10) 1 100
    where
      squarePipe :: (ℝ,ℝ,ℝ) -> ℝ -> ℝ -> SymbolicObj3
      squarePipe (x,y,z) diameter precision =
            union
            ((\(a, b, c)-> translate (V3 a b c)
                   $ rect3 (pure 0) (pure diameter)
                  )
             <$>
              zip3 (fmap (\n->(fromIntegral n/precision)*x) [0..100::Fastℕ])
                   (fmap (\n->(fromIntegral n/precision)*y) [0..100::Fastℕ])
                   (fmap (\n->(fromIntegral n/precision)*z) [0..100::Fastℕ]))

-- | A third 3d object to benchmark.
object3 :: SymbolicObj3
object3 =
    withRounding 1 $ difference (rect3 (pure (-1)) (pure 1)) [ rect3 (pure 0) (pure 2)]

-- | Example 13 - the rounded union of a cube and a sphere.
object4 :: SymbolicObj3
object4 = union [
                rect3 (pure 0) (pure 20),
                translate (pure 20) (sphere 15) ]

-- | Benchmark a 2D object.
obj2Benchmarks :: String -> String -> SymbolicObj2 -> Benchmark
obj2Benchmarks name filename obj =
    bgroup name
    [
      bench "SVG write" $ nfAppIO (writeSVG 1 $ filename <> ".svg") obj,
      bench "PNG write" $ nfAppIO (writePNG2 1 $ filename <> ".png") obj,
      bench "DXF write" $ nfAppIO (writeDXF2 1 $ filename <> ".dxf") obj,
      bench "Get contour" $ nf (symbolicGetContour 1 defaultObjectContext) obj
    ]

-- | Benchmark a 3D object.
obj3Benchmarks :: String -> String -> SymbolicObj3 -> Benchmark
obj3Benchmarks name filename obj =
    bgroup name
    [
--        bench "PNG write" $ writePNG3 1 "benchmark.png" obj
      bench "STLTEXT write" $ nfAppIO (writeSTL 1 $ filename <> ".stl.text") obj,
      bench "STL write" $ nfAppIO (writeBinSTL 1 $ filename <> ".stl") obj,
      bench "Get mesh" $ nf (symbolicGetMesh 1) obj
    ]

-- | Benchmark all of our objects.
benchmarks :: [Benchmark]
benchmarks =
    [ obj3Benchmarks "Object 1" "example5" object1
    , obj3Benchmarks "Object 2" "object2" object2
    , obj3Benchmarks "Object 3" "object3" object3
    , obj3Benchmarks "Object 4" "object4" object4
    , obj2Benchmarks "Object 2d 1" "example18" obj2d_1
    ]

-- | Our entrypoint. Runs all benchmarks.
main :: IO ()
main = defaultMain benchmarks

