-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

module Graphics.Implicit.Definitions where

-- Let's make things a bit nicer. 
-- Following math notation ℝ, ℝ², ℝ³...
type ℝ = Float
type ℝ2 = (ℝ,ℝ)
type ℝ3 = (ℝ,ℝ,ℝ)

type ℕ = Int

-- | A chain of line segments, as in SVG
-- eg. [(0,0), (0.5,1), (1,0)] ---> /\
type Polyline = [ℝ2]

-- | A triangle (a,b,c) = a trinagle with vertices a, b and c
type Triangle = (ℝ3, ℝ3, ℝ3)

-- | A triangle ((v1,n1),(v2,n2),(v3,n3)) has vertices v1, v2, v3
--   with corresponding normals n1, n2, and n3
type NormedTriangle = ((ℝ3, ℝ3), (ℝ3, ℝ3), (ℝ3, ℝ3))


-- | A triangle mesh is a bunch of triangles :)
type TriangleMesh = [Triangle]

-- | A normed triangle mesh is a bunch of normed trianlges!!
type NormedTriangleMesh = [NormedTriangle]

-- $ In Implicit CAD, we consider objects as functions
-- of `outwardness'. The boundary is 0, negative is the
-- interior and positive the exterior. The magnitude is
-- how far out or in.
-- For more details, refer to http://christopherolah.wordpress.com/2011/11/06/manipulation-of-implicit-functions-with-an-eye-on-cad/

-- | A 2D object
type Obj2 = (ℝ2 -> ℝ)

-- | A 3D object
type Obj3 = (ℝ3 -> ℝ)

-- | A 2D box
type Box2 = (ℝ2, ℝ2)

-- | A 3D box
type Box3 = (ℝ3, ℝ3)

-- | Boxed 2D object
type Boxed2 a = (a, Box2)

-- | Boxed 3D object
type Boxed3 a = (a, Box3)

type BoxedObj2 = Boxed2 Obj2
type BoxedObj3 = Boxed3 Obj3

-- | A symbolic 2D object format.
--   We want to have a symbolic object so that we can 
--   accelerate rendering & give ideal meshes for simple
--   cases.
data SymbolicObj2 =
	-- Primitives
	  RectR ℝ ℝ2 ℝ2
	| Circle ℝ
	| PolygonR ℝ [ℝ2]
	-- (Rounded) CSG
	| Complement2 SymbolicObj2
	| UnionR2 ℝ [SymbolicObj2]
	| DifferenceR2 ℝ [SymbolicObj2]
	| IntersectR2 ℝ [SymbolicObj2]
	-- Simple transforms
	| Translate2 ℝ2 SymbolicObj2
	| Scale2 ℝ SymbolicObj2
	| Rotate2 ℝ SymbolicObj2
	-- Boundary mods
	| Outset2 ℝ SymbolicObj2
	| Shell2 ℝ SymbolicObj2
	-- Misc
	| EmbedBoxedObj2 BoxedObj2
	deriving Show

-- | A symbolic 3D format!

data SymbolicObj3 = 
	-- Some simple primitives
	  Rect3R ℝ ℝ3 ℝ3
	| Sphere ℝ
	| Cylinder ℝ ℝ ℝ -- h r1 r2
	-- Some (rounded) CSG
	| Complement3 SymbolicObj3
	| UnionR3 ℝ [SymbolicObj3]
	| IntersectR3 ℝ [SymbolicObj3]
	| DifferenceR3 ℝ [SymbolicObj3]
	-- Some simple transofrms
	| Translate3 ℝ3 SymbolicObj3
	| Scale3 ℝ SymbolicObj3
	| Rotate3 (ℝ,ℝ,ℝ) SymbolicObj3
	-- Some boundary based transforms
	| Outset3 ℝ SymbolicObj3
	| Shell3 ℝ SymbolicObj3
	-- Misc
	| EmbedBoxedObj3 BoxedObj3
	-- 2D based
	| ExtrudeR ℝ SymbolicObj2 ℝ
	| ExtrudeRotateR ℝ ℝ SymbolicObj2 ℝ
	| ExtrudeRMod ℝ (ℝ -> ℝ2 -> ℝ2) SymbolicObj2 ℝ
	| ExtrudeOnEdgeOf SymbolicObj2 SymbolicObj2
	deriving Show

-- | Rectiliniar 2D set
type Rectiliniar2 = [Box2]

-- | Rectiliniar 2D set
type Rectiliniar3 = [Box3]

-- | Make ALL the functions Showable!
--   This is very handy when testing functions in interactive mode...
instance Show (a -> b) where
	show f = "<function>"

