ImplicitCAD: Math Inspired CAD
==============================

Introduction
------------

ImplicitCAD is a programmatic CAD program, implemented in haskell. Unlike traditional CAD programs, programmatic CAD programs use text descriptions of objects, as in programming. Concepts like variables, control structures and abstraction are used, just as in programming. This provides a number of advantages:

 - Objects can abstracted and reused
 - Repetitive tasks can be automated
 - Objects can be designed parametrically
 - The usual tools for software development (like version control) can be used

The traditional example of programmatic CAD is OpenSCAD.

Generally, objects in programmatic CAD are built with Constructive Solid Geometry or CSG. Unions, intersections and differences of simpler shapes slowly build the object. ImplicitCAD supports all this and much more! For example, it provides rounded unions so that one can have smooth interfaces between objects.

It also directly provides GCode generation, and has a parser for OpenSCAD to make it easier for people to transition.


Examples
---------

A simple 2D example:

```haskell
import Graphics.Implicit

out = union [
	square 80,
	translate (40,40) (circle 30) ]

writeSVG (-100,-100) (100,100) 2 "test.svg" out
``` 

![A Union of a Square and Circle](http://colah.github.com/ImplicitCADDocImages/0.0/SquareCircleUnion.png)


A rounded union:

```haskell
import Graphics.Implicit

out = unionR 14 [
	square 80,
	translate (40,40) (circle 30) ]

writeSVG (-100,-100) (100,100) 2 "test.svg" out
``` 

![A Rounded Union of a Square and Circle](http://colah.github.com/ImplicitCADDocImages/0.0/SquareCircleUnionR.png)

A simple 3D example:

```haskell
import Graphics.Implicit

out = union [
	cube 40,
	translate (20,20,20) (sphere 15) ]

writeSTL (-50,-50,-50) (50,50,50) 1 "test.stl" out 
```

![A Rounded Union of a Square and Circle](http://colah.github.com/ImplicitCADDocImages/0.0/CubeSphereUnion.png)

Try ImplicitCAD!
----------------

 1. Install GHC and cabal.
     * Debain/Ubuntu: `apt-get install ghc cabal-install`
     * Archlinux: `pacman -S ghc cabal-install`
     * Red Hat/Fedora: `yum install ghc cabal-install`
     * Mac OSX:
         * Homebrew: `brew install ghc cabal-install`
         * *Fink doesn't seem to have a package for cabal*
 2. Use cabal to install ImplicitCAD: `cabal update; cabal install implicit`
 3. Start ghci: `ghci`
 4. Load ImplicitCAD: `import Graphics.Implicit`
 5. Try it! `writeSVG (-35,-35) (35,35) 1 "test.svg" (circle 30)`

Documentation
-------------

Documentation can be generated from the source code of ImplicitCAD by Haddock by running `cabal haddock`.

Releases of ImplicitCAD are uploaded to HackageDB which, in addition to making them avaialable through `cabal install`, puts the generated documentation on the Internet. So you can read the documentation for the most recent release of ImplicitCAD, 0.0.0, [on HackageDB](http://hackage.haskell.org/packages/archive/implicit/0.0.0/doc/html/Graphics-Implicit.html).

A description of the mathematical ideas underpinning ImplicitCAD are described in a [blog post on colah's blog](http://christopherolah.wordpress.com/2011/11/06/manipulation-of-implicit-functions-with-an-eye-on-cad/).

Status
------

ImplicitCAD is very much a work in progress.

What works (Nov 2, 2011 -- regressions are possible if not probable):

 - CSG, bevelled CSG, shells.
 - 2D output (svg)
 - 3D output (stl) -- rare bugs that can be fixed by increasing re
 - gcode generation for 2D to hacklab laser cutter. Not configurable.

What still needs to be done:

 - openscad parser for backwards compatibility (partially completed)
 - gcode generation for 3D printers, gcode generator config

And a wishlist of things further in the future:

 - More optimisation
 - openGL viewer?
 - openCL acceleration?


