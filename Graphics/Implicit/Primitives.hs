-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright (C) 2014 2015 2016, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

-- FIXME: Required. why?
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

-- A module exporting all of the primitives, and some operations on them.
module Graphics.Implicit.Primitives (
                                     translate,
                                     mirror,
                                     scale,
                                     outset,
                                     complement, union, intersect, difference,
                                     unionR, intersectR, differenceR,
                                     shell,
                                     getBox,
                                     getImplicit,
                                     extrudeR,
                                     extrudeRM,
                                     extrudeRotateR,
                                     extrudeOnEdgeOf,
                                     sphere,
                                     cubeR, rect3R,
                                     circle,
                                     cylinder,
                                     cylinder2,
                                     squareR, rectR,
                                     polygonR,
                                     rotateExtrude,
                                     rotate3,
                                     rotateQ,
                                     rotate3V,
                                     pack3,
                                     rotate,
                                     pack2,
                                     implicit,
                                     emptySpace,
                                     fullSpace,
                                     _Shared,
                                     pattern Shared,
                                     Object
                                    ) where

import Prelude(Num, (+), (-), (*), (/), (.), negate, Bool(True, False), Maybe(Just, Nothing), Either, fmap, ($))

import Graphics.Implicit.Definitions (ℝ, ℝ2, ℝ3, Box2, SharedObj(..),
                                      SymbolicObj2(
                                                   SquareR,
                                                   Circle,
                                                   PolygonR,
                                                   Rotate2,
                                                   Shared2
                                                  ),
                                      SymbolicObj3(
                                                   CubeR,
                                                   Sphere,
                                                   Cylinder,
                                                   Rotate3,
                                                   ExtrudeR,
                                                   ExtrudeRotateR,
                                                   ExtrudeRM,
                                                   RotateExtrude,
                                                   ExtrudeOnEdgeOf,
                                                   Shared3
                                                  ),
                                      ExtrudeRMScale
                                     )
import Graphics.Implicit.MathUtil   (pack)
import Graphics.Implicit.ObjectUtil (getBox2, getBox3, getImplicit2, getImplicit3)
import Linear (V3(V3), axisAngle, Quaternion)
import Graphics.Implicit.Definitions (V2(V2))
import Control.Lens (prism', Prism', preview, (#))

-- $ 3D Primitives

sphere ::
    ℝ                  -- ^ Radius of the sphere
    -> SymbolicObj3    -- ^ Resulting sphere

sphere = Sphere

-- | A rectangular prism, with rounded corners.
rect3R ::
    ℝ                 -- ^ Rounding of corners
    -> ℝ3             -- ^ Bottom.. corner
    -> ℝ3             -- ^ Top right... corner
    -> SymbolicObj3   -- ^ Resuting cube

rect3R r xyz1 xyz2 = translate xyz1 $ CubeR r $ xyz2 - xyz1

-- | A rectangular prism, with rounded corners.
cubeR ::
    ℝ                 -- ^ Rounding of corners
    -> Bool           -- ^ Centered?
    -> ℝ3             -- ^ Size
    -> SymbolicObj3   -- ^ Resuting cube. (0,0,0) is bottom left if @center = False@,
                      -- otherwise it's the center.
cubeR r False size = CubeR r size
cubeR r True  size = translate (fmap (negate . (/ 2)) size) $ CubeR r size


-- | A conical frustum --- ie. a cylinder with different radii at either end.
cylinder2 ::
    ℝ                   -- ^ Radius of the cylinder
    -> ℝ                -- ^ Second radius of the cylinder
    -> ℝ                -- ^ Height of the cylinder
    -> SymbolicObj3     -- ^ Resulting cylinder

cylinder2 r1 r2 h = Cylinder h r1 r2

cylinder ::
    ℝ                   -- ^ Radius of the cylinder
    -> ℝ                -- ^ Height of the cylinder
    -> SymbolicObj3     -- ^ Resulting cylinder

cylinder r = cylinder2 r r

-- $ 2D Primitives

circle ::
    ℝ               -- ^ radius of the circle
    -> SymbolicObj2 -- ^ resulting circle

circle   = Circle

-- | A rectangle, with rounded corners.
rectR ::
    ℝ               -- ^ Rounding radius (in mm) of corners
    -> ℝ2           -- ^ Bottom left corner
    -> ℝ2           -- ^ Top right corner
    -> SymbolicObj2 -- ^ Resulting square

rectR r xy1 xy2 = translate xy1 $ SquareR r $ xy2 - xy1

-- | A rectangle, with rounded corners.
squareR ::
    ℝ               -- ^ Rounding radius (in mm) of corners
    -> Bool         -- ^ Centered?
    -> ℝ2           -- ^ Size
    -> SymbolicObj2 -- ^ Resulting square (bottom right = (0,0) )
squareR r False size = SquareR r size
squareR r True  size = translate (fmap (negate . (/ 2)) size) $ SquareR r size

-- | A 2D polygon, with rounded corners.
polygonR ::
    ℝ                -- ^ Rounding radius (in mm) of the polygon
    -> [ℝ2]          -- ^ Verticies of the polygon
    -> SymbolicObj2  -- ^ Resulting polygon

polygonR = PolygonR

-- $ Shared Operations

-- | Operations available on both 2D and 3D objects. The obvious omission of
-- rotation operations from this class are a technical limitation, and are
-- instead provided by 'rotate' and 'rotate3'.
--
-- Library users shouldn't need to provide new instances of this class.
class Num vec => Object obj vec
      | obj -> vec where

    -- | A 'Prism'' for including 'SharedObj's in @obj@. Prefer using 'Shared'
    -- instead of this.
    _Shared :: Prism' obj (SharedObj obj vec)

    -- | Get the bounding box an object
    getBox ::
        obj           -- ^ Object to get box of
        -> (vec, vec) -- ^ Bounding box

    -- | Get the implicit function for an object
    getImplicit ::
        obj           -- ^ Object to get implicit function of
        -> (vec -> ℝ) -- ^ Implicit function

-- | A pattern that abstracts over 'Shared2' and 'Shared3'.
pattern Shared :: Object obj vec => SharedObj obj vec -> obj
pattern Shared v <- (preview _Shared -> Just v)
  where
    Shared v = _Shared # v


-- | Translate an object by a vector of appropriate dimension.
translate
    :: Object obj vec
    => vec  -- ^ Vector to translate by
    -> obj  -- ^ Object to translate
    -> obj  -- ^ Resulting object
translate _ s@(Shared Empty) = s
translate _ s@(Shared Full) = s
translate v1 (Shared (Translate v2 s)) = translate (v1 + v2) s
translate v s = Shared $ Translate v s

-- | Scale an object
scale
    :: Object obj vec
    => vec  -- ^ Amount to scale by
    -> obj  -- ^ Object to scale
    -> obj  -- ^ Resulting scaled object
scale _ s@(Shared Empty) = s
scale v1 (Shared (Scale v2 s)) = scale (v1 * v2) s
scale v s = Shared $ Scale v s

-- | Complement an Object
complement
    :: Object obj vec
    => obj  -- ^ Object to complement
    -> obj  -- ^ Result
complement (Shared Empty) = Shared Full
complement (Shared Full) = Shared Empty
complement (Shared (Complement s)) = s
complement s = Shared $ Complement s

-- | The object that fills no space
emptySpace :: Object obj vec => obj
emptySpace = Shared Empty

-- | The object that fills the entire space
fullSpace :: Object obj vec => obj
fullSpace = Shared Full

-- | Mirror an object across the hyperplane whose normal is a given
-- vector.
mirror
    :: Object obj vec
    => vec  -- ^ Vector defining the hyperplane
    -> obj  -- ^ Object to mirror
    -> obj  -- ^ Resulting object
mirror _ s@(Shared Empty) = s
mirror _ s@(Shared Full) = s
mirror v s = Shared $ Mirror v s

-- | Outset of an object.
outset
    :: Object obj vec
    => ℝ        -- ^ distance to outset
    -> obj   -- ^ object to outset
    -> obj   -- ^ resulting object
outset _ s@(Shared Empty) = s
outset _ s@(Shared Full) = s
outset v1 (Shared (Outset v2 s)) = outset (v1 + v2) s
outset v s = Shared $ Outset v s

-- | Make a shell of an object.
shell
    :: Object obj vec
    => ℝ        -- ^ width of shell
    -> obj   -- ^ object to take shell of
    -> obj   -- ^ resulting shell
shell _ s@(Shared Empty) = s
shell _ s@(Shared Full) = s
shell v s = Shared $ Shell v s

-- | Rounded union
unionR
    :: Object obj vec
    => ℝ      -- ^ The radius (in mm) of rounding
    -> [obj]  -- ^ objects to union
    -> obj    -- ^ Resulting object
unionR _ [] = Shared Empty
unionR _ [s] = s
unionR r ss = Shared $ UnionR r ss

-- | Rounded difference
differenceR
    :: Object obj vec
    => ℝ        -- ^ The radius (in mm) of rounding
    -> obj   -- ^ Base object
    -> [obj] -- ^ Objects to subtract from the base
    -> obj   -- ^ Resulting object
differenceR _ s [] = s
differenceR _ s@(Shared Empty) _ = s
differenceR r s ss = Shared $ DifferenceR r s ss

-- | Rounded minimum
intersectR
    :: Object obj vec
    => ℝ     -- ^ The radius (in mm) of rounding
    -> [obj] -- ^ Objects to intersect
    -> obj   -- ^ Resulting object
intersectR _ [] = Shared Full
intersectR _ [s] = s
intersectR r ss = Shared $ IntersectR r ss

implicit
    :: Object obj vec
    => (vec -> ℝ)     -- ^ Implicit function
    -> (vec, vec)  -- ^ Bounding box
    -> obj         -- ^ Resulting object
implicit a b = Shared $ EmbedBoxedObj (a, b)


instance Object SymbolicObj2 ℝ2 where
  _Shared = prism' Shared2 $ \case
    Shared2 x -> Just x
    _         -> Nothing
  getBox      = getBox2
  getImplicit = getImplicit2

instance Object SymbolicObj3 ℝ3 where
  _Shared = prism' Shared3 $ \case
    Shared3 x -> Just x
    _         -> Nothing
  getBox      = getBox3
  getImplicit = getImplicit3


union :: Object obj vec => [obj] -> obj
union = unionR 0

difference :: Object obj vec => obj -> [obj] -> obj
difference = differenceR 0

intersect :: Object obj vec => [obj] -> obj
intersect = intersectR 0

-- 3D operations

-- | Extrude a 2d object upwards, with rounded corners.
extrudeR
    :: ℝ   -- ^ Rounding radius (in mm) of corners
    -> SymbolicObj2
    -> ℝ   -- ^ Extrusion height
    -> SymbolicObj3
extrudeR = ExtrudeR

-- | This function is not implemented
extrudeRotateR :: ℝ -> ℝ -> SymbolicObj2 -> ℝ -> SymbolicObj3
extrudeRotateR = ExtrudeRotateR

extrudeRM :: ℝ              -- ^ rounding radius (in mm)
    -> Either ℝ (ℝ -> ℝ)    -- ^ twist
    -> ExtrudeRMScale       -- ^ scale
    -> Either ℝ2 (ℝ -> ℝ2)  -- ^ translate
    -> SymbolicObj2         -- ^ object to extrude
    -> Either ℝ (ℝ2 -> ℝ)   -- ^ height to extrude to
    -> SymbolicObj3
extrudeRM = ExtrudeRM


rotateExtrude :: ℝ            -- ^ Angle to sweep to (in rad)
    -> Maybe ℝ                -- ^ Loop or path (rounded corner)
    -> Either ℝ2 (ℝ -> ℝ2)    -- ^ translate
    -> Either ℝ  (ℝ -> ℝ )    -- ^ rotate
    -> SymbolicObj2           -- ^ object to extrude
    -> SymbolicObj3
rotateExtrude = RotateExtrude

extrudeOnEdgeOf :: SymbolicObj2 -> SymbolicObj2 -> SymbolicObj3
extrudeOnEdgeOf = ExtrudeOnEdgeOf

-- | Rotate a 3D object via an Euler angle, measured in radians, along the
-- world axis.
rotate3 :: ℝ3 -> SymbolicObj3 -> SymbolicObj3
rotate3 (V3 pitch roll yaw)
  = Rotate3
  $ axisAngle (V3 0 0 1) yaw
  * axisAngle (V3 0 1 0) roll
  * axisAngle (V3 1 0 0) pitch

rotateQ
    :: Quaternion ℝ
    -> SymbolicObj3
    -> SymbolicObj3
rotateQ = Rotate3

-- | Rotate a 3D object along an arbitrary axis.
rotate3V
    :: ℝ   -- ^ Angle of rotation
    -> ℝ3  -- ^ Axis of rotation
    -> SymbolicObj3
    -> SymbolicObj3
rotate3V w xyz = Rotate3 $ axisAngle xyz w

-- FIXME: shouldn't this pack into a 3d area, or have a 3d equivalent?
-- | Attempt to pack multiple 3D objects into a fixed area. The @z@ coordinate
-- of each object is dropped, and the resulting packed objects will all be on
-- the same plane.
pack3
    :: ℝ2                  -- ^ Area to pack
    -> ℝ                   -- ^ Separation between objects
    -> [SymbolicObj3]      -- ^ Objects to pack
    -> Maybe SymbolicObj3  -- ^ 'Just' if the objects could be packed into the given area
pack3 (V2 dx dy) sep objs =
    let
        boxDropZ :: (ℝ3,ℝ3) -> (ℝ2,ℝ2)
        boxDropZ ((V3 a b _),(V3 d e _)) = (V2 a b, V2 d e)
        withBoxes :: [(Box2, SymbolicObj3)]
        withBoxes = fmap (\obj -> ( boxDropZ $ getBox3 obj, obj)) objs
    in case pack (V2 0 0,V2 dx dy) sep withBoxes of
            (a, []) -> Just $ union $ fmap (\((V2 x y),obj) -> translate (V3 x y 0) obj) a
            _ -> Nothing

-- 2D operations

rotate :: ℝ -> SymbolicObj2 -> SymbolicObj2
rotate = Rotate2

-- | Attempt to pack multiple 2D objects into a fixed area.
pack2
    :: ℝ2                  -- ^ Area to pack
    -> ℝ                   -- ^ Separation between objects
    -> [SymbolicObj2]      -- ^ Objects to pack
    -> Maybe SymbolicObj2  -- ^ 'Just' if the objects could be packed into the given area
pack2 (V2 dx dy) sep objs =
    let
        withBoxes :: [(Box2, SymbolicObj2)]
        withBoxes = fmap (\obj -> ( getBox2 obj, obj)) objs
    in case pack ((V2 0 0),(V2 dx dy)) sep withBoxes of
            (a, []) -> Just $ union $ fmap (\((V2 x y),obj) -> translate (V2 x y) obj) a
            _ -> Nothing

