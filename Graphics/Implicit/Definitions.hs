{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright 2014 2015 2016, 2017, 2018, Julia Longtin (julial@turinglace.com)
-- Copyright 2015 2016, Mike MacHenry (mike.machenry@gmail.com)
-- Released under the GNU AGPLV3+, see LICENSE

-- This module deliberately declares orphan instances of Show.
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DeriveGeneric #-}

-- Required. FIXME: why?
{-# LANGUAGE FlexibleInstances #-}

-- Definitions of the types used when modeling, and a few operators.
module Graphics.Implicit.Definitions (
    module F,
    module N,
    ℝ,
    ℝ2,
    both,
    ℝ3,
    allthree,
    minℝ,
    ComponentWiseMultable,
    (⋅),
    (⋯*),
    (⋯/),
    Polyline(Polyline),
    Polytri(Polytri),
    Triangle(Triangle),
    NormedTriangle(NormedTriangle),
    TriangleMesh(TriangleMesh),
    NormedTriangleMesh(NormedTriangleMesh),
    Obj2,
    Obj3,
    Box2,
    Box3,
    Boxed2,
    Boxed3,
    BoxedObj2,
    BoxedObj3,
    SharedObj(..),
    SymbolicObj2(
        SquareR,
        Circle,
        PolygonR,
        Rotate2,
        Shared2),
    SymbolicObj3(
        CubeR,
        Sphere,
        Cylinder,
        Rotate3,
        ExtrudeR,
        ExtrudeRotateR,
        ExtrudeRM,
        ExtrudeOnEdgeOf,
        RotateExtrude,
        Shared3),
    ExtrudeRMScale(C1, C2, Fn),
    fromℕtoℝ,
    fromFastℕtoℝ,
    fromℝtoFloat,
    toScaleFn,
    isScaleID,
    )
where

import GHC.Generics (Generic)

import Prelude (Semigroup((<>)), Monoid (mempty), Show, Double, Either(Left, Right), Bool(True, False), show, (*), (/), fromIntegral, Float, realToFrac)

import Data.Maybe (Maybe)

import Data.VectorSpace (Scalar, InnerSpace, (<.>))

import Graphics.Implicit.FastIntUtil as F (Fastℕ(Fastℕ), fromFastℕ, toFastℕ)

import Graphics.Implicit.IntegralUtil as N (ℕ, fromℕ, toℕ)

import Control.DeepSeq (NFData, rnf)

import Linear.Quaternion (Quaternion)

-- | A type synonym for 'Double'. When used in the context of positions or
-- sizes, measured in units of millimeters. When used as in the context of
-- a rotation, measured in radians.
type ℝ = Double

-- | A pair of two 'Double's. When used as an area or position vector, measured
-- in millimeters squared.
type ℝ2 = (ℝ,ℝ)

-- | A triple of 'Double's. When used as a volume or position vector, measured
-- in millimeters cubed. When used as a rotation, interpreted as Euler angles
-- measured in radians.
type ℝ3 = (ℝ,ℝ,ℝ)

-- | A give up point for dividing ℝs, and for the maximum difference between abs(n) and abs(-n).
minℝ :: ℝ
-- for Doubles.
minℝ = 0.0000000000000002
-- for Floats.
--minℝ = 0.00000011920928955078125 * 2

-- | apply a function to both items in the provided tuple.
both :: (t -> b) -> (t, t) -> (b, b)
both f (x,y) = (f x, f y)
{-# INLINABLE both #-}

-- | apply a function to all three items in the provided tuple.
allthree :: (t -> b) -> (t, t, t) -> (b, b, b)
allthree f (x,y,z) = (f x, f y, f z)
{-# INLINABLE allthree #-}

-- | TODO: Find a better place for this
(⋅) :: InnerSpace a => a -> a -> Scalar a
(⋅) = (<.>)
{-# INLINABLE (⋅) #-}

-- Wrap the functions that convert datatypes.

-- | Convert from our Integral to our Rational.
fromℕtoℝ :: ℕ -> ℝ
fromℕtoℝ = fromIntegral
{-# INLINABLE fromℕtoℝ #-}

-- | Convert from our Fast Integer (int32) to ℝ.
fromFastℕtoℝ :: Fastℕ -> ℝ
fromFastℕtoℝ (Fastℕ a) = fromIntegral a
{-# INLINABLE fromFastℕtoℝ #-}

-- | Convert from our rational to a float, for output to a file.
fromℝtoFloat :: ℝ -> Float
fromℝtoFloat = realToFrac
{-# INLINABLE fromℝtoFloat #-}

-- | add aditional instances to Show, for when we dump the intermediate form of objects.
--   FIXME: store functions in a dumpable form!
--   These instances cover functions
instance Show (ℝ -> ℝ) where
    show _ = "<function ℝ>"

instance Show (ℝ -> ℝ2) where
    show _ = "<expand ℝ -> ℝ2>"

instance Show (ℝ -> Either ℝ ℝ2) where
    show _ = "<function ℝ -> Either ℝ ℝ2>"

instance Show (ℝ2 -> ℝ) where
    show _ = "<collapse ℝ2 -> ℝ>"

instance Show (ℝ3 -> ℝ) where
    show _ = "<collapse ℝ3 -> ℝ>"

-- TODO: Find a better way to do this?
-- | Add multiply and divide operators for two ℝ2s or ℝ3s.
class ComponentWiseMultable a where
    (⋯*) :: a -> a -> a
    (⋯/) :: a -> a -> a
instance ComponentWiseMultable ℝ2 where
    (x,y) ⋯* (x',y') = (x*x', y*y')
    {-# INLINABLE (⋯*) #-}
    (x,y) ⋯/ (x',y') = (x/x', y/y')
    {-# INLINABLE (⋯/) #-}
instance ComponentWiseMultable ℝ3 where
    (x,y,z) ⋯* (x',y',z') = (x*x', y*y', z*z')
    {-# INLINABLE (⋯*) #-}
    (x,y,z) ⋯/ (x',y',z') = (x/x', y/y', z/z')
    {-# INLINABLE (⋯/) #-}

-- | A chain of line segments, as in SVG or DXF.
-- eg. [(0,0), (0.5,1), (1,0)] ---> /\
newtype Polyline = Polyline [ℝ2]

-- | A triangle in 2D space (a,b,c).
newtype Polytri = Polytri (ℝ2, ℝ2, ℝ2)

-- | A triangle in 3D space (a,b,c) = a triangle with vertices a, b and c
newtype Triangle = Triangle (ℝ3, ℝ3, ℝ3)

-- | A triangle ((v1,n1),(v2,n2),(v3,n3)) has vertices v1, v2, v3
--   with corresponding normals n1, n2, and n3
newtype NormedTriangle = NormedTriangle ((ℝ3, ℝ3), (ℝ3, ℝ3), (ℝ3, ℝ3))

-- | A triangle mesh is a bunch of triangles, attempting to be a surface.
newtype TriangleMesh = TriangleMesh [Triangle]

-- | A normed triangle mesh is a mesh of normed triangles.
newtype NormedTriangleMesh = NormedTriangleMesh [NormedTriangle]

instance NFData NormedTriangle where
  rnf (NormedTriangle ((a, na), (b, nb), (c, nc))) = rnf ((a, na), (b, nb), (c, nc))

instance NFData Triangle where
  rnf (Triangle (a,b,c)) = rnf (a,b,c)

instance NFData TriangleMesh where
  rnf (TriangleMesh xs) = rnf xs

instance NFData Polytri where
  rnf (Polytri (a,b,c)) = rnf (a,b,c)

instance NFData Polyline where
  rnf (Polyline xs) = rnf xs

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
--instance Show BoxedObj2 where
--    show _ = "<BoxedObj2>"

-- | A Boxed 3D object
type BoxedObj3 = Boxed3 Obj3
--instance Show BoxedObj3 where
--    show _ = "<BoxedObj3>"

data SharedObj obj vec
  = Complement obj
  | UnionR ℝ [obj]
  | DifferenceR ℝ obj [obj]
  | IntersectR ℝ [obj]
  | Translate vec obj
  | Scale vec obj
  | Mirror vec obj -- mirror across the line whose normal is defined by the R2
  | Outset ℝ obj
  | Shell ℝ obj
  | EmbedBoxedObj (vec -> ℝ, (vec, vec))
  deriving (Generic)

deriving instance (Show obj, Show vec, Show (vec -> ℝ))
  => Show (SharedObj obj vec)

-- | A symbolic 2D object format.
--   We want to have symbolic objects so that we can
--   accelerate rendering & give ideal meshes for simple
--   cases.
data SymbolicObj2 =
    -- Primitives
      SquareR ℝ ℝ2    -- rounding, size.
    | Circle ℝ        -- radius.
    | PolygonR ℝ [ℝ2] -- rounding, points.
    -- Simple transforms
    | Rotate2 ℝ SymbolicObj2
    -- Lifting common objects
    | Shared2 (SharedObj SymbolicObj2 ℝ2)
    deriving (Show, Generic)

-- | Semigroup under 'Graphic.Implicit.Primitives.union'.
instance Semigroup SymbolicObj2 where
  a <> b = Shared2 (UnionR 0 [a, b])

-- | Monoid under 'Graphic.Implicit.Primitives.union'.
instance Monoid SymbolicObj2 where
  mempty = SquareR 0 (0, 0)

-- | A symbolic 3D format!
data SymbolicObj3 =
    -- Primitives
      CubeR ℝ ℝ3 -- rounding, size.
    | Sphere ℝ -- radius
    | Cylinder ℝ ℝ ℝ --
    -- Simple transforms
    | Rotate3 (Quaternion ℝ) SymbolicObj3
    -- 2D based
    | ExtrudeR ℝ SymbolicObj2 ℝ
    | ExtrudeRotateR ℝ ℝ SymbolicObj2 ℝ
    | ExtrudeRM
        ℝ                     -- rounding radius
        (Either ℝ (ℝ -> ℝ))   -- twist
        ExtrudeRMScale        -- scale
        (Either ℝ2 (ℝ -> ℝ2)) -- translate
        SymbolicObj2          -- object to extrude
        (Either ℝ (ℝ2 -> ℝ))  -- height to extrude to
    | RotateExtrude
        ℝ                     -- Angle to sweep to
        (Maybe ℝ)             -- Loop or path (rounded corner)
        (Either ℝ2 (ℝ -> ℝ2)) -- translate
        (Either ℝ  (ℝ -> ℝ )) -- rotate
        SymbolicObj2          -- object to extrude
    | ExtrudeOnEdgeOf SymbolicObj2 SymbolicObj2
    | Shared3 (SharedObj SymbolicObj3 ℝ3)
    deriving (Show, Generic)

-- | Semigroup under 'Graphic.Implicit.Primitives.union'.
instance Semigroup SymbolicObj3 where
  a <> b = Shared3 (UnionR 0 [a, b])

-- | Monoid under 'Graphic.Implicit.Primitives.union'.
instance Monoid SymbolicObj3 where
  mempty = CubeR 0 (0, 0, 0)

data ExtrudeRMScale =
      C1 ℝ                  -- constant ℝ
    | C2 ℝ2                 -- constant ℝ2
    | Fn (ℝ -> Either ℝ ℝ2) -- function mapping height to either ℝ or ℝ2
    deriving (Show, Generic)

toScaleFn :: ExtrudeRMScale -> ℝ -> ℝ2
toScaleFn (C1 s) _ = (s, s)
toScaleFn (C2 s) _ = s
toScaleFn (Fn f) z = case f z of
    Left s -> (s, s)
    Right s -> s

isScaleID :: ExtrudeRMScale -> Bool
isScaleID (C1 1) = True
isScaleID (C2 (1, 1)) = True
isScaleID _ = False
