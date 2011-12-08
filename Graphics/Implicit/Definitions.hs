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

data SymbolicObj2 =
	Rect ℝ ℝ
	| Circle ℝ
	| Polygon [ℝ2]
	| UnionR2 ℝ [SymbolicObj2]
	| DifferenceR2 ℝ [SymbolicObj2]
	| IntersectionR2 ℝ [SymbolicObj2]
	| Translate2 SymbolicObj2
	| EmbedBoxedObj2 BoxedObj2

data SymbolicObj3 = 
	Rect3 ℝ ℝ ℝ
	| Sphere ℝ
	| UnionR3 ℝ [SymbolicObj3]
	| IntersectionR3 ℝ [SymbolicObj3]
	| DifferenceR3 ℝ [SymbolicObj3]
	| Translate3 SymbolicObj3
	| EmbedBoxedObj3 BoxedObj3

-- | Rectiliniar 2D set
type Rectiliniar2 = [Box2]

-- | Rectiliniar 2D set
type Rectiliniar3 = [Box3]

-- | Make ALL the functions Showable!
--   This is very handy when testing functions in interactive mode...
instance Show (a -> b) where
	show f = "<function>"

