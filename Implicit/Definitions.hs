-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

module Implicit.Definitions where

-- Let's make things a bit nicer. 
-- Following math notation ℝ, ℝ², ℝ³...
type ℝ = Float
type ℝ2 = (ℝ,ℝ)
type ℝ3 = (ℝ,ℝ,ℝ)

type ℕ = Int

-- A chain of line segments, as in SVG
-- eg. [(0,0), (0.5,1), (1,0)] ---> /\
type Polyline = [ℝ2]

-- In Implicit CAD, we consider objects as functions
-- of `outwardness'. The boundary is 0, negative is the
-- interior and positive the exterior. The magnitude is
-- how far out or in.

-- A 2D object
type Obj2 = (ℝ2 -> ℝ)

-- A 3D object
type Obj3 = (ℝ3 -> ℝ)



