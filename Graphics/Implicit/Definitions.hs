-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright 2014 2015 2016, 2017, 2018, Julia Longtin (julial@turinglace.com)
-- Copyright 2015 2016, Mike MacHenry (mike.machenry@gmail.com)
-- Released under the GNU AGPLV3+, see LICENSE

-- This module deliberately declares orphan instances of Show.
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- Allow us to use explicit foralls when writing function type declarations.
{-# LANGUAGE ExplicitForAll #-}

-- Required. FIXME: why?
{-# LANGUAGE FlexibleInstances #-}

-- Definitions of the types used when modeling, and a few operators.

module Graphics.Implicit.Definitions (
    module R,
    module N,
    ℝ2,
    both,
    ℝ3,
    allthree,
    (⋅),
    (⋯*),
    (⋯/),
    Polyline,
    Polytri,
    Triangle,
    NormedTriangle,
    TriangleMesh,
    NormedTriangleMesh,
    Obj2,
    Obj3,
    Box2,
    Box3,
    Boxed2,
    Boxed3,
    BoxedObj2,
    BoxedObj3,
    SymbolicObj2(
        RectR,
        Circle,
        PolygonR,
        Complement2,
        UnionR2,
        DifferenceR2,
        IntersectR2,
        Translate2,
        Scale2,
        Rotate2,
        Shell2,
        Outset2,
        EmbedBoxedObj2),
    SymbolicObj3(
        Rect3R,
        Sphere,
        Cylinder,
        Complement3,
        UnionR3,
        IntersectR3,
        DifferenceR3,
        Translate3,
        Scale3,
        Rotate3,
        Rotate3V,
        Shell3,
        Outset3,
        EmbedBoxedObj3,
        ExtrudeR,
        ExtrudeRotateR,
        ExtrudeRM,
        ExtrudeOnEdgeOf,
        RotateExtrude),
    Rectilinear2,
    Rectilinear3,
    )
where

import Prelude (Show, Either, show, (*), (/))

import Data.Maybe (Maybe)

import Data.VectorSpace (Scalar, InnerSpace, (<.>))

import Graphics.Implicit.RationalUtil as R (ℚ(minℝ, π, sqrt, cbrt, powℝ, powℝℝ, exp, log, cos, sin, tan, asin, acos, atan, sinh, cosh, tanh, atan2, fromℝ, toℝ, normalizeℝ, normalizeℝ2, normalizeℝ3, powℝ, (%), infty, neginfty), ℝ, fromFastℕtoℝ, fromℕtoℝ, fromℝtoℕ, fromℝtoFloat)

import Graphics.Implicit.IntegralUtil as N (ℕ, Fastℕ, fromℕ, toℕ, fromFastℕ, toFastℕ)

-- Following the math notation ℝ, ℝ², ℝ³...
type ℝ2 = (ℝ,ℝ)
type ℝ3 = (ℝ,ℝ,ℝ)

-- | apply a function to both items in the provided tuple.
both :: forall t b. (t -> b) -> (t, t) -> (b, b)
both f (x,y) = (f x, f y)

-- | apply a function to all three items in the provided tuple.
allthree :: forall t b. (t -> b) -> (t, t, t) -> (b, b, b)
allthree f (x,y,z) = (f x, f y, f z)

-- TODO: Find a better place for this
(⋅) :: InnerSpace a => a -> a -> Scalar a
(⋅) = (<.>)

-- add aditional instances to Show, for when we dump the intermediate form of an object.
instance Show (ℝ -> ℝ) where
    show _ = "<function ℝ>"

instance Show (ℝ -> ℝ2) where
    show _ = "<expand ℝ -> ℝ2>"

instance Show (ℝ2 -> ℝ) where
    show _ = "<collapse ℝ2 -> ℝ>"

instance Show (ℝ3 -> ℝ) where
    show _ = "<collapse ℝ3 -> ℝ>"

--instance Show BoxedObj2 where
--    show _ = "<BoxedObj2>"

--instance Show BoxedObj3 where
--    show _ = "<BoxedObj3>"

-- TODO: Find a better way to do this?
class ComponentWiseMultable a where
    (⋯*) :: a -> a -> a
    (⋯/) :: a -> a -> a
instance ComponentWiseMultable ℝ2 where
    (x,y) ⋯* (x',y') = (x*x', y*y')
    (x,y) ⋯/ (x',y') = (x/x', y/y')
instance ComponentWiseMultable ℝ3 where
    (x,y,z) ⋯* (x',y',z') = (x*x', y*y', z*z')
    (x,y,z) ⋯/ (x',y',z') = (x/x', y/y', z/z')

-- | A chain of line segments, as in SVG
-- eg. [(0,0), (0.5,1), (1,0)] ---> /\
type Polyline = [ℝ2]

-- | A triangle in 2D space (a,b,c).
type Polytri = (ℝ2, ℝ2, ℝ2)

-- | A triangle in 3D space (a,b,c) = a triangle with vertices a, b and c
type Triangle = (ℝ3, ℝ3, ℝ3)

-- | A triangle ((v1,n1),(v2,n2),(v3,n3)) has vertices v1, v2, v3
--   with corresponding normals n1, n2, and n3
type NormedTriangle = ((ℝ3, ℝ3), (ℝ3, ℝ3), (ℝ3, ℝ3))

-- | A triangle mesh is a bunch of triangles, attempting to be a surface.
type TriangleMesh = [Triangle]

-- | A normed triangle mesh is a mesh of normed triangles.
type NormedTriangleMesh = [NormedTriangle]

-- | A 2D object.
type Obj2 = (ℝ2 -> ℝ)

-- | A 3D object.
type Obj3 = (ℝ3 -> ℝ)

-- | A 2D box.
type Box2 = (ℝ2, ℝ2)

-- | A 3D box.
type Box3 = (ℝ3, ℝ3)

-- | A Box containing a 2D object.
type Boxed2 a = (a, Box2)

-- | A Box containing a 3D object.
type Boxed3 a = (a, Box3)

-- | A Boxed 2D object
type BoxedObj2 = Boxed2 Obj2

-- | A Boxed 3D object
type BoxedObj3 = Boxed3 Obj3

-- | A symbolic 2D object format.
--   We want to have symbolic objects so that we can
--   accelerate rendering & give ideal meshes for simple
--   cases.
data SymbolicObj2 =
    -- Primitives
      RectR ℝ ℝ2 ℝ2 -- rounding, start, stop.
    | Circle ℝ -- radius
    | PolygonR ℝ [ℝ2] -- rounding, points.
    -- (Rounded) CSG
    | Complement2 SymbolicObj2
    | UnionR2 ℝ [SymbolicObj2]
    | DifferenceR2 ℝ [SymbolicObj2]
    | IntersectR2 ℝ [SymbolicObj2]
    -- Simple transforms
    | Translate2 ℝ2 SymbolicObj2
    | Scale2 ℝ2 SymbolicObj2
    | Rotate2 ℝ SymbolicObj2
    -- Boundary mods
    | Outset2 ℝ SymbolicObj2
    | Shell2 ℝ SymbolicObj2
    -- Misc
    | EmbedBoxedObj2 BoxedObj2
    deriving Show

-- | A symbolic 3D format!
data SymbolicObj3 =
    -- Primitives
      Rect3R ℝ ℝ3 ℝ3 -- rounding, start, stop.
    | Sphere ℝ -- radius
    | Cylinder ℝ ℝ ℝ -- 
    -- (Rounded) CSG
    | Complement3 SymbolicObj3
    | UnionR3 ℝ [SymbolicObj3]
    | DifferenceR3 ℝ [SymbolicObj3]
    | IntersectR3 ℝ [SymbolicObj3]
    -- Simple transforms
    | Translate3 ℝ3 SymbolicObj3
    | Scale3 ℝ3 SymbolicObj3
    | Rotate3 ℝ3 SymbolicObj3
    | Rotate3V ℝ ℝ3 SymbolicObj3
    -- Boundary mods
    | Outset3 ℝ SymbolicObj3
    | Shell3 ℝ SymbolicObj3
    -- Misc
    | EmbedBoxedObj3 BoxedObj3
    -- 2D based
    | ExtrudeR ℝ SymbolicObj2 ℝ
    | ExtrudeRotateR ℝ ℝ SymbolicObj2 ℝ
    | ExtrudeRM
        ℝ                 -- rounding radius (ignored)
        (Maybe (ℝ -> ℝ))  -- twist
        (Maybe (ℝ -> ℝ))  -- scale
        (Maybe (ℝ -> ℝ2)) -- translate
        SymbolicObj2      -- object to extrude
        (Either ℝ (ℝ2 -> ℝ)) -- height to extrude to
    | RotateExtrude
        ℝ                     -- Angle to sweep to
        (Maybe ℝ)             -- Loop or path (rounded corner) (ignored)
        (Either ℝ2 (ℝ -> ℝ2)) -- translate function
        (Either ℝ  (ℝ -> ℝ )) -- rotate function
        SymbolicObj2          -- object to extrude
    | ExtrudeOnEdgeOf SymbolicObj2 SymbolicObj2
    deriving Show

-- | Rectilinear 2D set
type Rectilinear2 = [Box2]

-- | Rectilinear 3D set
type Rectilinear3 = [Box3]

