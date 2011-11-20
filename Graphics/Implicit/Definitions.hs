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

-- | Boxed 2D object
type BoxedObj2 = (Obj2, (ℝ2, ℝ2))

-- | Boxed 3D object
type BoxedObj3 = (Obj3, (ℝ3, ℝ3))

-- | Make ALL the functions Showable!
--   This is very handy when testing functions in interactive mode...
instance Show (a -> b) where
	show f = "<function>"

