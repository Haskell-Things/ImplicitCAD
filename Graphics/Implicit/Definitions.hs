{- ORMOLU_DISABLE -}
-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright 2014 2015 2016, 2017, 2018, Julia Longtin (julial@turinglace.com)
-- Copyright 2015 2016, Mike MacHenry (mike.machenry@gmail.com)
-- Released under the GNU AGPLV3+, see LICENSE

-- Required. FIXME: why?
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- required by makeWrapped
{-# LANGUAGE TypeOperators #-}

-- Definitions of the types used when modeling, and a few operators.
module Graphics.Implicit.Definitions (
    module F,
    module N,
    ℝ,
    ℝ2,
    ℝ3,
    ℝ3' (ℝ3'),
    minℝ,
    ComponentWiseMultable,
    (⋯*),
    (⋯/),
    Polyline(Polyline, getSegments),
    Polytri(Polytri),
    Triangle(Triangle),
    NormedTriangle(NormedTriangle),
    TriangleMesh(TriangleMesh, getTriangles),
    NormedTriangleMesh(NormedTriangleMesh, getNormedTriangles),
    Obj2,
    Obj3,
    Box2,
    Box3,
    Boxed2,
    Boxed3,
    BoxedObj2,
    BoxedObj3,
    SharedObj(..),
    V2(..),
    V3(..),
    SymbolicObj2(
        Square,
        Circle,
        Polygon,
        Rotate2,
        Transform2,
        Shared2),
    SymbolicObj3(
        Cube,
        Sphere,
        Cylinder,
        Rotate3,
        Transform3,
        Torus,
        Ellipsoid,
        Extrude,
        ExtrudeM,
        ExtrudeOnEdgeOf,
        RotateExtrude,
        Shared3),
    ExtrudeMScale(C1, C2, Fn),
    ObjectContext(..),
    defaultObjectContext,
    fromℕtoℝ,
    fromFastℕtoℝ,
    fromℝtoFloat,
    toScaleFn,
    isScaleID,
    quaternionToEuler,
    hasZeroComponent,
    )
where

import GHC.Generics (Generic)

import Prelude (Foldable, Num, Ord, Eq, atan2, asin, pi, (>=), signum, abs, (+), (-), RealFloat, (==), ($), flip, Semigroup((<>)), Monoid (mempty), Double, Either(Left, Right), Bool(True, False), (*), (/), fromIntegral, Float, realToFrac, (&&), RealFloat(isNaN), (||), any)

import Graphics.Implicit.FastIntUtil as F (Fastℕ(Fastℕ), fromFastℕ, toFastℕ)

import Graphics.Implicit.IntegralUtil as N (ℕ, fromℕ, toℕ)

import Control.DeepSeq (NFData, rnf)

import Linear (M33, M44, V2(V2), V3(V3))

import Linear.Quaternion (Quaternion(Quaternion))

import Control.Applicative (Applicative(liftA2))

import Text.Show.Combinators
    ( Show(showsPrec, show), (@|), showApp, showCon, PrecShowS)
import Control.Lens (makeWrapped)

-- | A type synonym for 'Double'. When used in the context of positions or
-- sizes, measured in units of millimeters. When used as in the context of
-- a rotation, measured in radians.
type ℝ = Double

-- | A pair of two 'Double's. When used as an area or position vector, measured
-- in millimeters squared.
type ℝ2 = V2 ℝ

-- | A triple of 'Double's. When used as a volume or position vector, measured
-- in millimeters cubed. When used as a rotation, interpreted as Euler angles
-- measured in radians.
type ℝ3 = V3 ℝ

-- ℝ3 except that we also check if values are NaN because those aren't
-- equal under the normal floating point equivalence.
newtype ℝ3' = ℝ3' (V3 ℝ)
$(makeWrapped ''ℝ3')
instance Eq ℝ3' where
  ℝ3' a == ℝ3' b = eqNaNs a b

eqNaNs :: ℝ3 -> ℝ3 -> Bool
eqNaNs (V3 a b c) (V3 a' b' c') =
    eqNaN a a' && eqNaN b b' && eqNaN c c'
eqNaN :: RealFloat a => a -> a -> Bool
eqNaN a b = (isNaN a && isNaN b) || (a == b)

-- | A give up point for dividing ℝs, and for the maximum difference between abs(n) and abs(-n).
minℝ :: ℝ
-- for Doubles.
minℝ = 0.0000000000000002
-- for Floats.
--minℝ = 0.00000011920928955078125 * 2

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

-- TODO: Find a better way to do this?
-- | Add multiply and divide operators for two ℝ2s or ℝ3s.
class ComponentWiseMultable a where
    (⋯*) :: a -> a -> a
    (⋯/) :: a -> a -> a
instance ComponentWiseMultable ℝ2 where
    (⋯*) = liftA2 (*)
    {-# INLINABLE (⋯*) #-}
    (⋯/) = liftA2 (/)
    {-# INLINABLE (⋯/) #-}
instance ComponentWiseMultable ℝ3 where
    (⋯*) = liftA2 (*)
    {-# INLINABLE (⋯*) #-}
    (⋯/) = liftA2 (/)
    {-# INLINABLE (⋯/) #-}

-- | A chain of line segments, as in SVG or DXF.
-- eg. [(0,0), (0.5,1), (1,0)] ---> /\
-- FIXME: May not be empty. expose to type system.
newtype Polyline = Polyline { getSegments :: [ℝ2] }

-- | A triangle in 2D space (a,b,c).
newtype Polytri = Polytri (ℝ2, ℝ2, ℝ2)

-- | A triangle in 3D space (a,b,c) = a triangle with vertices a, b and c
newtype Triangle = Triangle (ℝ3, ℝ3, ℝ3)

-- | A triangle ((v1,n1),(v2,n2),(v3,n3)) has vertices v1, v2, v3
--   with corresponding normals n1, n2, and n3
newtype NormedTriangle = NormedTriangle ((ℝ3, ℝ3), (ℝ3, ℝ3), (ℝ3, ℝ3))

-- | A triangle mesh is a bunch of triangles, attempting to be a surface.
newtype TriangleMesh = TriangleMesh { getTriangles :: [Triangle] }

-- | A normed triangle mesh is a mesh of normed triangles.
newtype NormedTriangleMesh = NormedTriangleMesh { getNormedTriangles :: [NormedTriangle] }

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

-- | Means of constructing symbolic objects that are common between the 2D and
-- 3D case. This type is parameterized on @obj@ and @vec@ so that
-- 'SymbolicObj2' and 'SymbolicObj3' can instantiate it for their own purposes.
data SharedObj obj f a
  = Empty  -- ^ The empty object
  | Full   -- ^ The entirely full object
  | Complement obj
  | UnionR ℝ [obj]
  | DifferenceR ℝ obj [obj]
  | IntersectR ℝ [obj]
  | Translate (f a) obj
  | Scale (f a) obj
  | Mirror (f a) obj -- ^ Mirror across the line whose normal is defined by the vector
  | Outset ℝ obj
  | Shell ℝ obj
  | EmbedBoxedObj (f a -> a, (f a, f a))
  | WithRounding ℝ obj
  deriving (Generic)

instance (Show obj, Show (f a)) => Show (SharedObj obj f a) where
  showsPrec = flip $ \case
     Empty                   -> showCon "emptySpace"
     Full                    -> showCon "fullSpace"
     Complement obj          -> showCon "complement"   @| obj
     UnionR 0 l_obj          -> showCon "union"        @| l_obj
     UnionR r l_obj          -> showCon "unionR"       @| r   @| l_obj
     DifferenceR 0 obj l_obj -> showCon "difference"   @| obj @| l_obj
     DifferenceR r obj l_obj -> showCon "differenceR"  @| r   @| obj @| l_obj
     IntersectR 0 l_obj      -> showCon "intersect"    @| l_obj
     IntersectR r l_obj      -> showCon "intersectR"   @| r   @| l_obj
     Translate vec obj       -> showCon "translate"    @| vec @| obj
     Scale vec obj           -> showCon "scale"        @| vec @| obj
     Mirror vec obj          -> showCon "mirror"       @| vec @| obj
     Outset r obj            -> showCon "outset"       @| r   @| obj
     Shell r obj             -> showCon "shell"        @| r   @| obj
     EmbedBoxedObj (_, box)  -> showCon "implicit"     @| Blackhole @| box
     WithRounding r obj      -> showCon "withRounding" @| r   @| obj

------------------------------------------------------------------------------
-- | A type whose show instance is a hole @_@. Used for giving 'Show' instances
-- to data types which contain functions or other unshowable things.
data Blackhole = Blackhole

instance Show Blackhole where
  show _ = "_"

newtype ObjectContext = ObjectContext
  { objectRounding :: ℝ
  } deriving (Eq, Ord, Show)

defaultObjectContext :: ObjectContext
defaultObjectContext = ObjectContext
  { objectRounding = 0
  }

-- | A symbolic 2D object format.
--   We want to have symbolic objects so that we can
--   accelerate rendering & give ideal meshes for simple
--   cases.
data SymbolicObj2 =
    -- Primitives
      Square ℝ2     -- size.
    | Circle ℝ      -- radius.
    | Polygon [ℝ2]  -- points.
    -- Simple transforms
    | Rotate2 ℝ SymbolicObj2
    | Transform2 (M33 ℝ) SymbolicObj2
    -- Lifting common objects
    | Shared2 (SharedObj SymbolicObj2 V2 ℝ)
    deriving (Generic)

instance Show SymbolicObj2 where
  showsPrec = flip $ \case
    -- NB: The False here is the centering argument, which has already been
    -- transformed into a translate. The 'Square' constructor itself is never
    -- centered.
    Square sz        -> showCon "square"     @| False @| sz
    Circle r         -> showCon "circle"     @| r
    Polygon ps       -> showCon "polygon"    @| ps
    Rotate2 v obj    -> showCon "rotate"     @| v     @| obj
    Transform2 m obj -> showCon "transform"  @| m     @| obj
    Shared2 obj   -> flip showsPrec obj

-- | Semigroup under 'Graphic.Implicit.Primitives.union'.
instance Semigroup SymbolicObj2 where
  a <> b = Shared2 (UnionR 0 [a, b])

-- | Monoid under 'Graphic.Implicit.Primitives.union'.
instance Monoid SymbolicObj2 where
  mempty = Shared2 Empty

-- | A symbolic 3D format!
data SymbolicObj3 =
    -- Primitives
      Cube ℝ3 -- rounding, size.
    | Sphere ℝ -- radius
    | Cylinder ℝ ℝ ℝ --
    | Torus ℝ ℝ
    | Ellipsoid ℝ ℝ ℝ
    -- Simple transforms
    | Rotate3 (Quaternion ℝ) SymbolicObj3
    | Transform3 (M44 ℝ) SymbolicObj3
    -- 2D based
    | Extrude ℝ SymbolicObj2
    | ExtrudeM
        (Either ℝ (ℝ -> ℝ))   -- twist
        ExtrudeMScale         -- scale
        (Either ℝ2 (ℝ -> ℝ2)) -- translate
        SymbolicObj2          -- object to extrude
        (Either ℝ (ℝ2 -> ℝ))  -- height to extrude to
    | RotateExtrude
        ℝ                     -- Angle to sweep to
        (Either ℝ2 (ℝ -> ℝ2)) -- translate
        (Either ℝ  (ℝ -> ℝ )) -- rotate
        SymbolicObj2          -- object to extrude
    | ExtrudeOnEdgeOf SymbolicObj2 SymbolicObj2
    | Shared3 (SharedObj SymbolicObj3 V3 ℝ)
    deriving (Generic)

instance Show SymbolicObj3 where
  showsPrec = flip $ \case
    -- NB: The False here is the centering argument, which has already been
    -- transformed into a translate. The 'Cube' constructor itself is never
    -- centered.
    Cube sz -> showCon "cube" @| False @| sz
    Sphere d -> showCon "sphere" @| d
    -- NB: The arguments to 'Cylinder' are backwards compared to 'cylinder' and
    -- 'cylinder2'.
    Cylinder h r1 r2 | r1 == r2 ->
      showCon "cylinder" @| r1 @| h
    Cylinder h r1 r2 ->
      showCon "cylinder2" @| r1 @| r2 @| h
    Rotate3 qd s -> showCon "rotate3" @| quaternionToEuler qd @| s
    Transform3 m s -> showCon "transform3" @| m @| s
    Extrude d2 s -> showCon "extrude" @| d2 @| s
    ExtrudeM edfdd e ep_ddfdp_dd s edfp_ddd ->
      showCon "extrudeM" @|| edfdd @| e @|| ep_ddfdp_dd @| s @|| edfp_ddd
    RotateExtrude d ep_ddfdp_dd edfdd s ->
      showCon "rotateExtrude" @| d @|| ep_ddfdp_dd @|| edfdd @| s
    ExtrudeOnEdgeOf s s1 ->
      showCon "extrudeOnEdgeOf" @| s @| s1
    Shared3 s -> flip showsPrec s
    Torus r1 r2 -> showCon "torus" @| r1 @| r2
    Ellipsoid a b c -> showCon "ellipsoid" @| a @| b @| c

infixl 2 @||
------------------------------------------------------------------------------
-- | ImplicitCAD uses the pattern @Either a (b -> c)@ for many of its
-- higher-order arguments. The left case is for constant values, but the right
-- side is for things that should vary. Since we can't show functions, ths
-- combinator works like '(@|)' except that it shows the left case and uses
-- a hole for the right.
(@||) :: Show a => PrecShowS -> Either a (b -> c) -> PrecShowS
showF @|| x = showApp showF $ case x of
  Left a  -> showCon "Left" @| a
  Right _ -> showCon "Right" @| Blackhole

-- | Semigroup under 'Graphic.Implicit.Primitives.union'.
instance Semigroup SymbolicObj3 where
  a <> b = Shared3 (UnionR 0 [a, b])

-- | Monoid under 'Graphic.Implicit.Primitives.union'.
instance Monoid SymbolicObj3 where
  mempty = Shared3 Empty

data ExtrudeMScale =
      C1 ℝ                  -- constant ℝ
    | C2 ℝ2                 -- constant ℝ2
    | Fn (ℝ -> Either ℝ ℝ2) -- function mapping height to either ℝ or ℝ2
    deriving (Generic)

instance Show ExtrudeMScale where
  showsPrec = flip $ \case
    C1 r  -> showCon "C1" @| r
    C2 r2 -> showCon "C2" @| r2
    Fn _  -> showCon "Fn" @| Blackhole

toScaleFn :: ExtrudeMScale -> ℝ -> ℝ2
toScaleFn (C1 s) _ = V2 s s
toScaleFn (C2 s) _ = s
toScaleFn (Fn f) z = case f z of
    Left s -> V2 s s
    Right s -> s

isScaleID :: ExtrudeMScale -> Bool
isScaleID (C1 1) = True
isScaleID (C2 (V2 1 1)) = True
isScaleID _ = False

-- | Convert a 'Quaternion' to its constituent euler angles.
--
-- From https://en.wikipedia.org/wiki/Conversion_between_quaternions_and_Euler_angles#Source_code_2
quaternionToEuler :: RealFloat a => Quaternion a -> V3 a
quaternionToEuler (Quaternion w (V3 x y z))=
  let sinr_cosp = 2 * (w * x + y * z)
      cosr_cosp = 1 - 2 * (x * x + y * y)
      sinp = 2 * (w * y - z * x);
      siny_cosp = 2 * (w * z + x * y);
      cosy_cosp = 1 - 2 * (y * y + z * z);
      pitch = if abs sinp >= 1
              then signum sinp * pi / 2
              else asin sinp
      roll = atan2 sinr_cosp cosr_cosp
      yaw = atan2 siny_cosp cosy_cosp
   in V3 roll pitch yaw
{-# INLINABLE quaternionToEuler #-}

-- | Returns True if any component of a foldable functor is zero
hasZeroComponent
    :: (Foldable f, Num a, Eq a)
    => f a
    -> Bool
{-# INLINABLE hasZeroComponent #-}
hasZeroComponent =  any (==0)
