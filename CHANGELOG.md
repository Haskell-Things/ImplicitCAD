
# Version [next](https://github.com/colah/ImplicitCAD/compare/v0.3.0.0...master) (202Y-MM-DD)

* Changelog started. Previous release was `0.3.0.1`.

* Haskell interface changes
  * Added support for XY-scaling on `linear_extrude` [#269](https://github.com/colah/ImplicitCAD/pull/269)
    * `ExtrudeRM` scale type changes from `(Either ℝ (ℝ -> ℝ))` to `ExtrudeRMScale`
  * Replaced the Rect primitives with SquareR and CubeR [#296](https://github.com/colah/ImplicitCAD/pull/296)
    * `squareR` and `cubeR` are now available
  * Added support for mirroring objects around axis using `mirror` function [#300](https://github.com/colah/ImplicitCAD/pull/300)
  * `differenceR` changes type to require mandatory shape to substract from [#294](https://github.com/colah/ImplicitCAD/pull/294)
    * from `Object obj vec => R -> [obj] -> obj`
    * to `Object obj vec => R -> obj -> [obj] -> obj`
  * Both `SymbolicObj2` and `SymbolicObj3` now have `Semigroup` and `Monoid` instances, where `<>` acts as `union` [#301](https://github.com/colah/ImplicitCAD/pull/301)

* ExtOpenSCAD interface changes
  * `scale` function with single parameter now behaves similar to OpenSCAD one [#258](https://github.com/colah/ImplicitCAD/pull/258)
    * scales 2D object in both dimensions
  * `rotateExtrude` angle `a` parameter renamed to `angle` to match OpenSCAD [#259](https://github.com/colah/ImplicitCAD/pull/259)
  * Added `mirror` support [#300](https://github.com/colah/ImplicitCAD/pull/300)

* Other changes
  * Fixed the ExtOpenSCAD lexer bug where newlines were part of identifiers [#256](https://github.com/colah/ImplicitCAD/pull/256)
  * `implicitsnap` not built by default anymore [#272](https://github.com/colah/ImplicitCAD/pull/272)
    * Can be enabled again with `cabal configure --flag=implicitsnap`
  * Fixed vertex coordinates of OBJ output [#281](https://github.com/colah/ImplicitCAD/pull/281)
    * `discreteAprox` of `NormedTriangleMesh` now runs in parallel [#282](https://github.com/colah/ImplicitCAD/pull/282)
  * Binaries now built with default `-rtsopts "-with-rtsopts -N -qg -t"` to allow automatic parallelization
  * Added haddocks for Haskell eDSL [#284](https://github.com/colah/ImplicitCAD/pull/284) & [#287](https://github.com/colah/ImplicitCAD/pull/287)
  * Added golden test machinery [#311](https://github.com/colah/ImplicitCAD/pull/311)
  * Added quickcheck test machinery for implicit functions [#316](https://github.com/colah/ImplicitCAD/pull/316)
  * Rotate now internally uses quaternions [#314](https://github.com/colah/ImplicitCAD/pull/314)
