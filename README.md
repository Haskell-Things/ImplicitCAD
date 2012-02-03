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


ExtOpenSCAD Examples
--------------------

Let's being with OpenSCAD examples, since they're likely a more comfortable format that Haskell for most readers :)

ImplicitCAD supports an extended version of OpenSCAD, the popular programmatic CAD tool. This is very new functionality, so expect bugs!

Generally, normal openscad code should work. For example, save the following as `file.scad`.

```c
union() {
	square([80,80]);
	translate ([40,40]) circle(30);
}
``` 

Running `extopenscad file.scad` will produce `file.svg`, which will look like:

![A Union of a Square and Circle](http://colah.github.com/ImplicitCADDocImages/0.0/SquareCircleUnion.png)

You can read more about standard openscad functionality in [OpenSCAD User Manual](http://en.wikibooks.org/wiki/OpenSCAD_User_Manual). 

However, there are additional ImplicitCAD specific features. For example a rounded union:

```c
union(r=14) {
	square([80,80]);
	translate ([40,40]) circle(30);
}
``` 

![A Rounded Union of a Square and Circle](http://colah.github.com/ImplicitCADDocImages/0.0/SquareCircleUnionR.png)

Like openscad, ImplicitCAD supports extruding objects.

```c
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

And we allow you to twist them as you extrude.

![An Extrusion](http://colah.github.com/ImplicitCADDocImages/0.0/ExtrudeObj.png)

```c
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

![An twisted extrusion](http://colah.github.com/ImplicitCADDocImages/0.0/ExtrudeTwistObj.png)

In fact, we've extended this to allow you to twist at non-constant rates and even reverse directions. You just make `twist` a function! (We're following the openscad convention of using degrees...)

```c
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

![An variably twisted ImplicitCAD extrusion](http://colah.github.com/ImplicitCADDocImages/0.0/ExtrudeVarTwistObj.png)

We also allow you to do rounded extrusions. See, we heard you like rounding, so we set this up so you can rounded extrude your rounded union...

```c
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

![A rounded extrusion made with ImplicitCAD](http://colah.github.com/ImplicitCADDocImages/0.0/ExtrudeRoundObj.png)

This is fully compatible with twisting, of course!

```c
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

![A rounded twisted extrusion](http://colah.github.com/ImplicitCADDocImages/0.0/ExtrudeRoundTwistObj.png)


ImplicitCAD also provides full programmatic functionality, like variable assignment in loops, which are sadly absent in OpenSCAD. For example, the trivial program:

```c
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
f = max(4);
echo(f(5));
echo(max(4,5));
```
And some higher order functions, like my friend map:

```c
echo(map(cos, [0, pi/2, pi]));
```


Haskell Examples
-----------------

Everything you saw above can be done with the Haskell API. For example, a simple 2D example, the same as our first ExtOpenSCAD one:

```haskell
import Graphics.Implicit

out = union [
	square 80,
	translate (40,40) (circle 30) ]

main = writeSVG 2 "test.svg" out
``` 

![A Union of a Square and Circle](http://colah.github.com/ImplicitCADDocImages/0.0/SquareCircleUnion.png)


A rounded union:

```haskell
import Graphics.Implicit

out = unionR 14 [
	square 80,
	translate (40,40) (circle 30) ]

main = writeSVG 2 "test.svg" out
``` 

![A Rounded Union of a Square and Circle](http://colah.github.com/ImplicitCADDocImages/0.0/SquareCircleUnionR.png)

A simple 3D example:

```haskell
import Graphics.Implicit

out = union [
	cube 40,
	translate (20,20,20) (sphere 15) ]

main = writeSTL 1 "test.stl" out 
```

![A Rounded Union of a Square and Circle](http://colah.github.com/ImplicitCADDocImages/0.0/CubeSphereUnion.png)

You can do a whole lot more!

Try ImplicitCAD!
----------------

Install development branch (*recommended*):

 1. Install GHC and cabal (see above)
 2. Git clone this repo: `git clone https://github.com/colah/ImplicitCAD.git`
 3. cd in: `cd ImplicitCAD/`
 4. cabal install it: `cabal install`
 5. Start ghci: `ghci`
 6. Load ImplicitCAD: `import Graphics.Implicit`
 7. Try it! `writeSVG (-35,-35) (35,35) 1 "test.svg" (circle 30)`


(*Development branch only:* If you want to use the extended OpenSCAD interpreter, extopenscad, you may need to modify your `$PATH` variable to include `~/.cabal/bin/`.)

Install latest stable release (*out of date, examples in README may not work*):

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

A description of the mathematical ideas underpinning ImplicitCAD are described in a [blog post on colah's blog](http://christopherolah.wordpress.com/2011/11/06/manipulation-of-implicit-functions-with-an-eye-on-cad/). Note that substantial changes have happened since that post.

Status
------

ImplicitCAD is very much a work in progress.

What works (Nov 2, 2011 -- regressions are possible if not probable):

 - CSG, bevelled CSG, shells.
 - 2D output (svg)
 - 3D output (stl) -- rare bugs that can be fixed by increasing re
 - gcode generation for 2D to hacklab laser cutter. Not configurable.


What still needs to be done:

 - gcode generation for 3D printers, gcode generator config
 - openscad parser for backwards compatibility (partially complete)

And a wishlist of things further in the future:

 - More optimisation
 - openGL viewer?
 - openCL acceleration?


