ImplicitCAD: Math Inspired CAD
==============================

[![Hackage version](https://img.shields.io/hackage/v/implicit.svg?style=flat)](https://hackage.haskell.org/package/implicit)  [![Build Status](https://travis-ci.org/colah/ImplicitCAD.png?branch=master)](https://travis-ci.org/colah/ImplicitCAD)

Introduction
------------

ImplicitCAD is a programmatic CAD program, implemented in haskell. Unlike traditional CAD programs, programmatic CAD programs use text descriptions of objects, as in programming. Concepts like variables, control structures and abstraction are used, just as in programming. This provides a number of advantages:

 - Objects can be abstracted and reused
 - Repetitive tasks can be automated
 - Objects can be designed parametrically
 - The usual tools for software development (like version control) can be used

The traditional example of programmatic CAD is OpenSCAD.

Generally, objects in programmatic CAD are built with Constructive Solid Geometry or CSG. Unions, intersections and differences of simpler shapes slowly build the object. ImplicitCAD supports all this and much more! For example, it provides rounded unions so that one can have smooth interfaces between objects.

It also directly provides some GCode generation, and has a parser for OpenSCAD to make it easier for people to transition/use.

ImplicitCAD is very much a work in progress. The author considers it ready for beta testers and greatly appreciates bug reports.


ExtOpenSCAD Examples
--------------------

Let's begin with OpenSCAD examples, since they're likely a more comfortable format than Haskell for most readers :)

ImplicitCAD supports a modified version of the OpenSCAD language, used by the popular programmatic CAD tool of the same name.

Generally, normal OpenSCAD code should work. For example, save the following as `example1.scad` (or grab it out of the Examples/ directory shipped with ImplicitCAD).

```c
// example1.scad -- The union of a square and a circle.
union() {
	square([80,80]);
	translate ([80,80]) circle(30);
}
```

Running `extopenscad example1.scad` will produce `example1.svg`, which will look like:

![A Union of a Square and Circle](http://faikvm.com/ImplicitCAD/example1.svg)

You can read more about standard openscad functionality in the [OpenSCAD User Manual](http://en.wikibooks.org/wiki/OpenSCAD_User_Manual).

However, there are additional ImplicitCAD specific features. For example a rounded union:

```c
//example2.escad -- A rounded union of a square and a circle.
union(r=14) {
	square([80,80]);
	translate ([80,80]) circle(30);
}
```

![A Rounded Union of a Square and Circle](http://faikvm.com/ImplicitCAD/example2.svg)

(For code like this that is not backwards compatible with OpenSCAD, it is recommended that you save it as a .escad file -- Extended OpenSCAD.)

Like openscad, ImplicitCAD supports extruding objects.

```c
// example3.escad -- the extruded product of the union of five circles.
linear_extrude (height = 40, center=true){
        union ( r = 8) {
                circle (10);
                translate ([22,0]) circle (10);
                translate ([0,22]) circle (10);
                translate ([-22,0]) circle (10);
                translate ([0,-22]) circle (10);
        }
}
```

![An Extrusion](http://faikvm.com/ImplicitCAD/example3.png)

And we allow you to twist them as you extrude.


```c
// example4.escad -- the twisted extruded product of the union of five circles.
linear_extrude (height = 40, center=true, twist=90){
        union ( r = 8) {
                circle (10);
                translate ([22,0]) circle (10);
                translate ([0,22]) circle (10);
                translate ([-22,0]) circle (10);
                translate ([0,-22]) circle (10);
        }
}
```

![An twisted extrusion](http://faikvm.com/ImplicitCAD/example4.png)

In fact, we've extended this to allow you to twist at non-constant rates and even reverse directions. You just make `twist` a function! (We're following the openscad convention of using degrees...)

```c
// example5.escad -- the variably twisted extruded product of the union of 5 circles.
linear_extrude (height = 40, center=true, twist(h) = 35*cos(h*2*pi/60)) {
        union ( r = 8) {
                circle (10);
                translate ([22,0]) circle (10);
                translate ([0,22]) circle (10);
                translate ([-22,0]) circle (10);
                translate ([0,-22]) circle (10);
        }
}
```

![A variably twisted ImplicitCAD extrusion](http://faikvm.com/ImplicitCAD/example5.png)

We also allow you to do rounded extrusions. See, we heard you like rounding, so we set this up so you can rounded extrude your rounded union...

```c
// example6.escad -- A rounded extrusion of the rounded union of 5 circles.
linear_extrude (height = 40, center=true, r=5){
        union ( r = 8) {
                circle (10);
                translate ([22,0]) circle (10);
                translate ([0,22]) circle (10);
                translate ([-22,0]) circle (10);
                translate ([0,-22]) circle (10);
        }
}
```

![A rounded extrusion made with ImplicitCAD](http://faikvm.com/ImplicitCAD/example6.png)

This is fully compatible with twisting, of course!

```c
// example7.escad -- A twisted rounded extrusion of the rounded union of 5 circles.
linear_extrude (height = 40, center=true, twist=90, r=5){
        union ( r = 8) {
                circle (10);
                translate ([22,0]) circle (10);
                translate ([0,22]) circle (10);
                translate ([-22,0]) circle (10);
                translate ([0,-22]) circle (10);
        }
}
```

![A rounded twisted extrusion](http://faikvm.com/ImplicitCAD/example7.png)


ImplicitCAD also provides full programmatic functionality, like variable assignment in loops, which are sadly absent in OpenSCAD. For example, the trivial program:

```c
// Example8.escad -- variable assignment in loops.
a = 5;
for (c = [1, 2, 3]) {
	echo(c);
	a = a*c;
	echo(a);
}
```

Has the output:


```
1.0
5.0
2.0
10.0
3.0
30.0
Nothing to render
```

As a functional programmer, I couldn't resist adding some other niceties to the language. For example, function currying:

```c
// Example9.escad -- function currying.
f = max(4);
echo(f(5));
echo(max(4,5));
```
And some higher order functions, like my friend map:

```c
// Example10.escad -- map!.
echo(map(cos, [0, pi/2, pi]));
```

Haskell Examples
-----------------

Everything you saw above can be done with the Haskell API. For example, a simple 2D example, the same as our first ExtOpenSCAD one:

```haskell
-- Example 11 - the union of a square and a circle.
import Graphics.Implicit

out = union [
	rectR 0 (-40,-40) (40,40),
	translate (40,40) (circle 30) ]

main = writeSVG 2 "test.svg" out
```

![A Union of a Square and a Circle](http://faikvm.com/ImplicitCAD/example11.svg)


A rounded union:

```haskell
-- Example 12 - the rounded union of a square and a circle.
import Graphics.Implicit

out = unionR 14 [
	rectR 0 (-40,-40) (40,40),
	translate (40,40) (circle 30) ]

main = writeSVG 2 "test.svg" out
```

![A Rounded Union of a Square and a Circle](http://faikvm.com/ImplicitCAD/example12.svg)

A simple 3D example:

```haskell
-- Example 13 - the union of a cube and a sphere.
import Graphics.Implicit

out = union [
	rect3R 0 (0,0,0) (20,20,20),
	translate (20,20,20) (sphere 15) ]

main = writeSTL 1 "test.stl" out
```

![A Rounded Union of a Cube and a Sphere](http://faikvm.com/ImplicitCAD/example13.png)

You can do a whole lot more!

Try ImplicitCAD!
----------------

 1. Install GHC and cabal.
     * Debain/Ubuntu: `apt-get install ghc cabal-install zlib1g-dev`
     * Archlinux: `pacman -S ghc cabal-install`
     * Red Hat/Fedora: `yum install ghc cabal-install`
     * Mac OSX:
         * Homebrew: `brew install ghc cabal-install`
         * *Fink doesn't seem to have a package for cabal*; Install the Haskell Platform [manually](http://hackage.haskell.org/platform/mac.html).
     * Windows: Follows [these install instructions](http://hackage.haskell.org/platform/windows.html).
     * Other unices: If your package manager does not include ghc and cabal you should [install the Haskell platform](http://www.haskell.org/platform).
 2. You now have two options for installation:
     * Latest release:
         * Use cabal to install ImplicitCAD: `cabal update && cabal install implicit`
     * Development version:
         * Initialize your haskell environment: `cabal update`
         * Git clone this repo: `git clone https://github.com/colah/ImplicitCAD.git`
         * cd in: `cd ImplicitCAD/`
         * install the dependencies: `cabal configure && cabal install --only-dependencies`
	     * The previous step may fail, but it should tell you what's missing.
	     * try to 'cabal install' each of the things it tells you are missing.
         * Finally, cabal install implicitcad: `cabal install`
 3. Try it!
     * extopenscad test:
          * Make a test file: `echo "circle(30);" > test.escad`
          * Run extopencad: `extopenscad test.escad`
             * Alternatively, `~/.cabal/bin/extopenscad test.escad` -- see bellow.
     * Haskell ImplicitCAD test:
          * Start ghci: `ghci`
          * Load ImplicitCAD: `import Graphics.Implicit`
          * Try it! `writeSVG 1 "test.svg" (circle 30)`
 4. Known issues:
     * extopenscad test results in `bash: extopenscad: command not found` (or similar for your shell)
         * This probably means `~/.cabal/bin/` is not in your `$PATH` variable.
           Try using `~/.cabal/bin/extopenscad` as your command instead.
     * Haskell test results in `module is not loaded: 'Graphics.Implicit' (./Graphics/Implicit.hs)`
         * This is most likely a problem with your Linux distro and cabal not playing nice.
           GHC is not configured to see the ImplicitCAD libraries. You can confirm this by
           trying the test in `~/.cabal/lib/`. If that works, you should be able to use ghc
           anywhere with the `-Ldir` or `-llib` options. Alternatively, some people have
           permanently fixed this by doing the cabal install as root.

Documentation
-------------

Documentation can be generated from the source code of ImplicitCAD by Haddock by running `cabal haddock`.

Releases of ImplicitCAD are uploaded to HackageDB which, in addition to making them avaialable through `cabal install`, puts the generated documentation on the Internet. So you can read the documentation for the most recent release of ImplicitCAD, 0.0.1, [on HackageDB](http://hackage.haskell.org/packages/archive/implicit/0.0.3/doc/html/Graphics-Implicit.html) (for some reason the latest version doesn't seem to have built).

In Implicit CAD, we consider objects as functions of `outwardness'. The boundary is 0, negative is the interior and positive the exterior. The magnitude is how far out or in. A description of the mathematical ideas underpinning ImplicitCAD are in a [blog post on colah's blog](http://christopherolah.wordpress.com/2011/11/06/manipulation-of-implicit-functions-with-an-eye-on-cad/). Note that substantial changes have happened since that post. You can also look at the [0.0.3 relase notes](http://christopherolah.wordpress.com/2012/02/06/implicitcad-0-0-3-release/).

Status
------

ImplicitCAD is very much a work in progress.

What works (July 31, 2015 -- regressions are possible if not probable):

 - CSG, bevelled CSG, shells.
 - 2D output (svg).
 - 3D output (stl).
 - gcode generation for 2D to hacklab laser cutter. Not configurable.

What still needs to be done:

 - gcode generation for 3D printers, gcode generator config.
 - openscad parser for backwards compatibility (partially complete).

And a wishlist of things further in the future:

 - More optimisation.
 - Less bugs.
 - openGL viewer?
 - openCL acceleration?
