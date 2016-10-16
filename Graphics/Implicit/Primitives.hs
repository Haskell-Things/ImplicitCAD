-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright (C) 2014 2015 2016, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

-- Allow us to use explicit foralls when writing function type declarations.
{-# LANGUAGE ExplicitForAll #-}

-- Required.
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, TypeSynonymInstances, FlexibleInstances #-}

-- A module exporting all of the primitives, and some operations on them.
module Graphics.Implicit.Primitives (
                                     translate,
                                     scale,
                                     complement, union, intersect, difference,
                                     unionR, intersectR, differenceR,
                                     shell,
                                     extrudeR,
                                     extrudeRM,
                                     extrudeRotateR,
                                     extrudeOnEdgeOf,
                                     sphere,
                                     rect3R,
                                     circle,
                                     cylinder,
                                     cylinder2,
                                     rectR,
                                     polygonR,
                                     rotateExtrude,
                                     rotate3,
                                     rotate3V,
                                     pack3,
                                     rotate,
                                     pack2,
                                     implicit
                                    ) where

import Graphics.Implicit.Definitions (ℝ, ℝ2, ℝ3, Box2,
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
                                                   Outset2,
                                                   Shell2,
                                                   EmbedBoxedObj2
                                                  ),
                                      SymbolicObj3(
                                                   Rect3R,
                                                   Sphere,
                                                   Cylinder,
                                                   Complement3,
                                                   UnionR3,
                                                   DifferenceR3,
                                                   IntersectR3,
                                                   Translate3,
                                                   Scale3,
                                                   Rotate3,
                                                   Rotate3V,
                                                   Outset3,
                                                   Shell3,
                                                   EmbedBoxedObj3,
                                                   ExtrudeR,
                                                   ExtrudeRotateR,
                                                   ExtrudeRM,
                                                   RotateExtrude,
                                                   ExtrudeOnEdgeOf
                                                  )
                                     )
import Graphics.Implicit.MathUtil   (pack)
import Graphics.Implicit.ObjectUtil (getBox2, getBox3, getImplicit2, getImplicit3)

-- $ 3D Primitives

sphere ::
    ℝ                  -- ^ Radius of the sphere
    -> SymbolicObj3    -- ^ Resulting sphere

sphere = Sphere

rect3R ::
    ℝ                 -- ^ Rounding of corners
    -> ℝ3             -- ^ Bottom.. corner
    -> ℝ3             -- ^ Top right... corner
    -> SymbolicObj3   -- ^ Resuting cube - (0,0,0) is bottom left...

rect3R = Rect3R

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

rectR ::
    ℝ
    -> ℝ2           -- ^ Bottom left corner
    -> ℝ2           -- ^ Top right corner
    -> SymbolicObj2 -- ^ Resulting square (bottom right = (0,0) )

rectR = RectR

polygonR ::
    ℝ                -- ^ Rouding of the polygon
    -> [ℝ2]          -- ^ Verticies of the polygon
    -> SymbolicObj2  -- ^ Resulting polygon

polygonR = PolygonR

-- $ Shared Operations

class Object obj vec | obj -> vec where
    
    -- | Complement an Object
    complement ::
        obj     -- ^ Object to complement
        -> obj  -- ^ Result
    
    -- | Rounded union
    unionR ::
        ℝ        -- ^ The radius of rounding
        -> [obj] -- ^ objects to union
        -> obj   -- ^ Resulting object

    -- | Rounded difference
    differenceR ::
        ℝ        -- ^ The radius of rounding
        -> [obj] -- ^ Objects to difference
        -> obj   -- ^ Resulting object

    -- | Rounded minimum
    intersectR ::
        ℝ        -- ^ The radius of rounding
        -> [obj] -- ^ Objects to intersect
        -> obj   -- ^ Resulting object
    
    -- | Translate an object by a vector of appropriate dimension.
    translate ::
        vec      -- ^ Vector to translate by (Also: a is a vector, blah, blah)
        -> obj   -- ^ Object to translate
        -> obj   -- ^ Resulting object

    -- | Scale an object
    scale ::
        vec     -- ^ Amount to scale by
        -> obj  -- ^ Object to scale
        -> obj  -- ^ Resulting scaled object

    -- | Outset an object.
    outset ::
        ℝ        -- ^ distance to outset
        -> obj   -- ^ object to outset
        -> obj   -- ^ resulting object

    -- | Make a shell of an object.
    shell ::
        ℝ        -- ^ width of shell
        -> obj   -- ^ object to take shell of
        -> obj   -- ^ resulting shell

    -- | Get the bounding box an object
    getBox ::
        obj           -- ^ Object to get box of
        -> (vec, vec) -- ^ Bounding box

    -- | Get the implicit function for an object
    getImplicit ::
        obj           -- ^ Object to get implicit function of
        -> (vec -> ℝ) -- ^ Implicit function

    implicit ::
        (vec -> ℝ)     -- ^ Implicit function
        -> (vec, vec)  -- ^ Bounding box
        -> obj         -- ^ Resulting object
    

instance Object SymbolicObj2 ℝ2 where
    translate   = Translate2
    scale       = Scale2
    complement  = Complement2
    unionR      = UnionR2
    intersectR  = IntersectR2
    differenceR = DifferenceR2
    outset      = Outset2
    shell       = Shell2
    getBox      = getBox2
    getImplicit = getImplicit2
    implicit a b= EmbedBoxedObj2 (a,b)

instance Object SymbolicObj3 ℝ3 where
    translate   = Translate3
    scale       = Scale3
    complement  = Complement3
    unionR      = UnionR3
    intersectR  = IntersectR3
    differenceR = DifferenceR3
    outset      = Outset3
    shell       = Shell3
    getBox      = getBox3
    getImplicit = getImplicit3
    implicit a b= EmbedBoxedObj3 (a,b)

union :: forall obj vec. Object obj vec => [obj] -> obj
union = unionR 0

difference :: forall obj vec. Object obj vec => [obj] -> obj
difference = differenceR 0

intersect :: forall obj vec. Object obj vec => [obj] -> obj
intersect = intersectR 0

-- 3D operations

extrudeR :: ℝ -> SymbolicObj2 -> ℝ -> SymbolicObj3
extrudeR = ExtrudeR

extrudeRotateR :: ℝ -> ℝ -> SymbolicObj2 -> ℝ -> SymbolicObj3
extrudeRotateR = ExtrudeRotateR

extrudeRM :: ℝ
    -> Maybe (ℝ -> ℝ)
    -> Maybe (ℝ -> ℝ)
    -> Maybe (ℝ -> ℝ2)
    -> SymbolicObj2
    -> Either ℝ (ℝ2 -> ℝ)
    -> SymbolicObj3
extrudeRM = ExtrudeRM

rotateExtrude :: ℝ
    -> Maybe ℝ
    -> Either ℝ2 (ℝ -> ℝ2)
    -> Either ℝ (ℝ -> ℝ)
    -> SymbolicObj2
    -> SymbolicObj3
rotateExtrude = RotateExtrude

extrudeOnEdgeOf :: SymbolicObj2 -> SymbolicObj2 -> SymbolicObj3
extrudeOnEdgeOf = ExtrudeOnEdgeOf

rotate3 :: (ℝ, ℝ, ℝ) -> SymbolicObj3 -> SymbolicObj3
rotate3 = Rotate3

rotate3V :: ℝ -> ℝ3 -> SymbolicObj3 -> SymbolicObj3
rotate3V = Rotate3V

-- FIXME: shouldn't this pack into a 3d area, or have a 3d equivalent?
pack3 :: ℝ2 -> ℝ -> [SymbolicObj3] -> Maybe SymbolicObj3
pack3 (dx, dy) sep objs =
    let
        boxDropZ ((a,b,_),(d,e,_)) = ((a,b),(d,e))
        withBoxes :: [(Box2, SymbolicObj3)]
        withBoxes = map (\obj -> ( boxDropZ $ getBox3 obj, obj)) objs
    in case pack ((0,0),(dx,dy)) sep withBoxes of
            (a, []) -> Just $ union $ map (\((x,y),obj) -> translate (x,y,0) obj) a
            _ -> Nothing

-- 2D operations

rotate :: ℝ -> SymbolicObj2 -> SymbolicObj2
rotate = Rotate2

pack2 :: ℝ2 -> ℝ -> [SymbolicObj2] -> Maybe SymbolicObj2
pack2 (dx, dy) sep objs =
    let
        withBoxes :: [(Box2, SymbolicObj2)]
        withBoxes = map (\obj -> ( getBox2 obj, obj)) objs
    in case pack ((0,0),(dx,dy)) sep withBoxes of
            (a, []) -> Just $ union $ map (\((x,y),obj) -> translate (x,y) obj) a
            _ -> Nothing

