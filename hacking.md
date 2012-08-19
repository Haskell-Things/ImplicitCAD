
ImplicitCAD Hacking How To
==========================

So you want to improve ImplicitCAD. Yay! More help is a good thing.

As of the time of writing, ImplicitCAD has 3417 lines of code, 896 lines of comments, and 877 blank lines, for a total of 5190 lines spread over 42 files. For a project of ImplicitCAD's scope, that's pretty small, but it's still enough that it can be difficult to find the section we need to change...

The structure of ImplicitCAD is as follows:

```
Graphics
└── Implicit
    ├── Export
    │   ├── Render
    │   └── Symbolic
    ├── ExtOpenScad
    │   └── Util
    └── ObjectUtil
```

`Graphics.Implicit.Export` is, as you may guess, where all the export stuff is. `Graphics.Implicit.ExtOpenScad` is the programming language interpreter for the ExtOpenScad language, our extention of openscad. Finally, the graphics engine is defined in `Graphics.Implicit` and `Graphics.Implicit.ObjectUtil`.

The rest of this file will go through different changes you are likely to want to make and how to implement them.

Language Changes
----------------

Most likely, you want to change one of four things:

* **Expressions**: Expressions are things like `1+2`, `"abc"`, and `[sin(3.14), pi]`. They are defined in `Graphics.Implicit.ExtOpenScad.Expressions`. (Note that `sin` and `pi` are variables, which are defined elsewhere.)

* **Statements**: Statements are things like variable assignment, for loops, and if statements. For example `for (a = [1,2,3]) echo (a);`. Statements are defined in `Graphics.Implicit.ExtOpenScad.Statements`.

```haskell
computationStatement = ...
			ifStatement,
			forStatement,
			...

...

forStatement = (do
	line <- fmap sourceLine getPosition
	-- a for loop is of the form:
	--      for ( vsymb = vexpr   ) loopStatements
	-- eg.  for ( a     = [1,2,3] ) {echo(a);   echo "lol";}
	-- eg.  for ( [a,b] = [[1,2]] ) {echo(a+b); echo "lol";}
	string "for"
	many space
	char '('
	many space
	pattern <- patternMatcher
	many space
	char '='
	vexpr <- expression 0
	char ')'
	many space
	loopStatements <- suite
	...
```


* **Default Variables**: Like `sin`, `pi`, `sqrt`. These are all defined in ` Graphics.Implicit.ExtOpenScad.Default`. We can just use `toOObj` to convert Haskell values into `OpenscadObj`s and use them as default variable settings. (Small caveat: inputs to `toOObj` can't be polymorphic, so we use a type signature to force it to a certain type.)

```haskell
defaultFunctions = map (\(a,b) -> (a, toOObj ( b :: ℝ -> ℝ)))
	[
		("sin",   sin),
		("cos",   cos),
		("tan",   tan),
		...
	]
```

* **Default Modules**: Like `sphere` and `linear_extrude`. These are all defined in `Graphics.Implicit.ExtOpenScad.Primitives`.

```haskell
primitives = [ sphere, cube, square, cylinder, ... ]

...

-- **Exmaple of implementing a module**
-- sphere is a module without a suite named sphere,
-- this means that the parser will look for this like
--       sphere(args...);
sphere = moduleWithoutSuite "sphere" $ do
	example "sphere(3);"
	example "sphere(r=5);"
	-- What are the arguments?
	-- The radius, r, which is a (real) number.
	-- Because we don't provide a default, this ends right
	-- here if it doesn't get a suitable argument!
	r :: ℝ <- argument "r" 
	            `doc` "radius of the sphere"
	-- So what does this module do?
	-- It adds a 3D object, a sphere of radius r,
	-- using the sphere implementation in Prim
	-- (Graphics.Implicit.Primitives)
	addObj3 $ Prim.sphere r

```

Output Formats
--------------

Formats are defined in files like `Graphics.Implicit.Export.TriangleMeshFormats` (as is the case with STLs), `Graphics.Implicit.Export.PolylineMeshFormats` (as is the case with SVGs).

Then, in `Graphics.Implicit.Export`:

```haskell
writeSVG res = writeObject res PolylineFormats.svg
writeSTL res = writeObject res  TriangleMeshFormats.stl
```

Rendering Algorithms
--------------------

These are defined in `Graphics.Implicit.Export.Render` and children. `Graphics.Implicit.Export.Render` begins with an outline of how rendering is done:

```haskell
-- Here's the plan for rendering a cube (the 2D case is trivial):

-- (1) We calculate midpoints using interpolate.
--     This guarentees that our mesh will line up everywhere.
--     (Contrast with calculating them in getSegs)

import Graphics.Implicit.Export.Render.Interpolate (interpolate)

...
```

If you are interested on working on this part of the code, read it. The children are also well documented.

Graphics Primitives
-------------------

The most complicated part of ImplicitCAD is the actual graphics engine. Before working on it, please familiarize yourself with the theory as described in [Chris' blog post](http://christopherolah.wordpress.com/2011/11/06/manipulation-of-implicit-functions-with-an-eye-on-cad/) (though changes have occured since then).

The simples way to implement a new primitive is using `implicit`, a contructor that takes an implicit function and boudning box, producing an object. For example, we could have originally defined `sphere` as:

```haskell
sphere :: ℝ -> SymbolicObj3
sphere r = implicit (
	\(x,y,z) -> sqrt (x^2+y^2+z^2) - r,
	((-r, -r, -r), (r, r, r))
	)
```

and put it in `Graphics.Implicit.Primitives`. However, to allow more powerful optimizations, meta-inspection, and other goodies, frequently used objects should be put in the `SymbolicObj` definitions in `Graphics.Implicit.Definitions`. For example, `sphere`:

```haskell
data SymbolicObj3 = 
	  Rect3R ℝ ℝ3 ℝ3
	| Sphere ℝ
	...
```

Then one needs to make the relevant entries in `Graphics.Implicit.ObjectUtil.*`.

`Graphics.Implicit.ObjectUtil.Box3`:

```haskell
getBox3 (Sphere r ) = ((-r, -r, -r), (r,r,r))
```

`Graphics.Implicit.ObjectUtil.GetImplicit3`:

```haskell
getImplicit3 (Sphere r ) = 
	\(x,y,z) -> sqrt (x**2 + y**2 + z**2) - r
```


