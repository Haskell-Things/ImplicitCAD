
ImplicitCAD: Math Inspired CAD
==============================

Intro
-----

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

> import Implicit
> 
> out = union 
> 	(square 40) 
> 	(translate (20,20) (circle 15) )
>
> writeSVG (-50,-50) (50,50) 1 "test.svg" out 

A rounded union:

> import Implicit
> 
> out = unionR 7
> 	(square 40) 
> 	(translate (20,20) (circle 15) )
>
> writeSVG (-50,-50) (50,50) 1 "test.svg" out 


A simple 3D example:

> import Implicit
> 
> out = union 
> 	(cube 40) 
> 	(translate (20,20,20) (sphere 15) )
>
> writeSTL (-50,-50,-50) (50,50,50) 1 "test.stl" out 


Status
------

ImplicitCAD is very much a work in progress.

What works (Oct 24, 2011 -- regressions are possible if not probable):

 - CSG, bevelled CSG, shells.
 - 2D output (svg)
 - 3D output (stl) -- rare bugs that can be fixed by increasing re
 - gcode generation for 2D to hacklab laser cutter. Not configurable.

What still needs to be done:

 - openscad parser for backwards compatibility (partially completed)
 - Documentation
 - Finish openscad parser
 - More primitives, operations
 - gcode generation for 3D printers, gcode generator config
 - parallelize marching cubes, etc for 3D mesh generation (2D already done)
 - openGL viewer?
 - openCL acceleration?


