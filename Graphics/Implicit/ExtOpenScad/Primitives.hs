-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright 2016, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

-- Idealy, we'd like to parse a superset of openscad code, with some improvements.

-- This file provides primitive objects for the openscad parser.

-- FIXME: why are these required?
{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}

-- Allow us to use shorter forms of Var and Name.
{-# LANGUAGE PatternSynonyms #-}

-- For the type arithmatic involved in calling VectorSpace.
{-# LANGUAGE TypeFamilies #-}

-- to simplify polygon
{-# LANGUAGE PatternGuards #-}

-- Export one set containing all of the primitive object's patern matches.
module Graphics.Implicit.ExtOpenScad.Primitives (primitives) where

import Prelude(String, IO, Either(Left, Right), Bool(False), Maybe(Just, Nothing), ($), return, either, id, (-), (==), (&&), (<), (*), cos, sin, pi, (/), (>), const, uncurry, fmap, fromInteger, round, (/=), (||), not, null, map, (++), putStrLn, otherwise)

import Graphics.Implicit.Definitions (ℝ, ℝ2, ℝ3, ℕ, SymbolicObj2, SymbolicObj3, fromℕtoℝ)

import Graphics.Implicit.ExtOpenScad.Definitions (OVal (OObj2, OObj3), ArgParser, Symbol(Symbol))

import Graphics.Implicit.ExtOpenScad.Util.ArgParser (doc, defaultTo, example, test, eulerCharacteristic)

import qualified Graphics.Implicit.ExtOpenScad.Util.ArgParser as GIEUA (argument)

import Graphics.Implicit.ExtOpenScad.Util.OVal (OTypeMirror, caseOType, divideObjs, (<||>))

-- Note the use of a qualified import, so we don't have the functions in this file conflict with what we're importing.
import qualified Graphics.Implicit.Primitives as Prim (sphere, rect3R, rectR, translate, circle, polygonR, extrudeR, cylinder2, union, unionR, intersect, intersectR, difference, differenceR, rotate, rotate3V, rotate3, scale, extrudeR, extrudeRM, rotateExtrude, shell, pack3, pack2)

import Data.Maybe (isNothing)

import Control.Monad (mplus)

import Data.VectorSpace (VectorSpace, Scalar, (*^))

import Data.AffineSpace (distanceSq)

default (ℝ)

-- | Use the old syntax when defining arguments.
argument :: forall desiredType. (OTypeMirror desiredType) => String -> ArgParser desiredType
argument a = GIEUA.argument (Symbol a)

-- | The only thing exported here. basically, a list of functions, which accept OVal arguments and return an ArgParser ?
-- | FIXME: allow for these to fail, and return a failure condition.
primitives :: [(Symbol, [OVal] -> ArgParser (IO [OVal]))]
primitives = [ sphere, cube, square, cylinder, circle, polygon, union, difference, intersect, translate, scale, rotate, extrude, pack, shell, rotateExtrude, unit ]

-- | sphere is a module without a suite.
--   this means that the parser will look for this like
--   sphere(args...);
sphere :: (Symbol, [OVal] -> ArgParser (IO [OVal]))
sphere = moduleWithoutSuite "sphere" $ do
    example "sphere(3);"
    example "sphere(r=5);"
    -- arguments:
    -- The radius, r, which is a (real) number.
    -- Because we don't provide a default, this ends right
    -- here if it doesn't get a suitable argument!
    r :: ℝ <- argument "r"
                `doc` "radius of the sphere"
    -- This module adds a 3D object, a sphere of radius r,
    -- using the sphere implementation in Prim
    -- (Graphics.Implicit.Primitives)
    addObj3 $ Prim.sphere r

-- | FIXME: square1, square2 like cylinder has?
-- | FIXME: translate for square2?
cube :: (Symbol, [OVal] -> ArgParser (IO [OVal]))
cube = moduleWithoutSuite "cube" $ do
    -- examples
    example "cube(size = [2,3,4], center = true, r = 0.5);"
    example "cube(4);"
    -- arguments (two forms)
    ((x1,x2), (y1,y2), (z1,z2)) <-
        do
            x :: Either ℝ ℝ2 <- argument "x"
                `doc` "x or x-interval"
            y :: Either ℝ ℝ2 <- argument "y"
                `doc` "y or y-interval"
            z :: Either ℝ ℝ2 <- argument "z"
                `doc` "z or z-interval"
            center :: Bool <- argument "center"
                `doc` "should center? (non-intervals)"
                `defaultTo` False
            let
                toInterval' :: ℝ -> ℝ2
                toInterval' = toInterval center
            return (either toInterval' id x,
                    either toInterval' id y,
                    either toInterval' id z)
        <|> do
            size   :: Either ℝ ℝ3  <- argument "size"
                `doc`  "cube size"
            center :: Bool <- argument "center"
                `doc` "should center?"
                `defaultTo` False
            let (x,y, z) = either (\w -> (w,w,w)) id size
            return (toInterval center x, toInterval center y, toInterval center z)
    -- arguments shared between forms
    r      :: ℝ    <- argument "r"
                        `doc` "radius of rounding"
                        `defaultTo` 0
    -- Tests
    test "cube(4);"
        `eulerCharacteristic` 2
    test "cube(size=[2,3,4]);"
        `eulerCharacteristic` 2
    test "cube([2,3,4]);" -- openscad syntax
        `eulerCharacteristic` 2
    addObj3 $ Prim.rect3R r (x1, y1, z1) (x2, y2, z2)

square :: (Symbol, [OVal] -> ArgParser (IO [OVal]))
square = moduleWithoutSuite "square" $ do
    -- examples
    example "square(x=[-2,2], y=[-1,5]);"
    example "square(size = [3,4], center = true, r = 0.5);"
    example "square(4);"
    -- arguments shared between forms
    r      :: ℝ    <- argument "r"
                        `doc` "radius of rounding"
                        `defaultTo` 0
    -- arguments (two forms)
    ((x1,x2), (y1,y2)) <-
        do
            x :: Either ℝ ℝ2 <- argument "x"
                `doc` "x or x-interval"
            y :: Either ℝ ℝ2 <- argument "y"
                `doc` "y or y-interval"
            center :: Bool <- argument "center"
                `doc` "should center? (non-intervals)"
                `defaultTo` False
            let
                toInterval' :: ℝ -> ℝ2
                toInterval' = toInterval center
            return (either toInterval' id x,
                    either toInterval' id y)
        <|> do
            size   :: Either ℝ ℝ2  <- argument "size"
                `doc`  "square size"
            center :: Bool <- argument "center"
                `doc` "should center?"
                `defaultTo` False
            let (x,y) = either (\w -> (w,w)) id size
            return (toInterval center x, toInterval center y)
    -- Tests
    test "square(2);"
        `eulerCharacteristic` 0
    test "square(size=[2,3]);"
        `eulerCharacteristic` 0
    addObj2 $ Prim.rectR r (x1, y1) (x2, y2)

cylinder :: (Symbol, [OVal] -> ArgParser (IO [OVal]))
cylinder = moduleWithoutSuite "cylinder" $ do
    example "cylinder(r=10, h=30, center=true);"
    example "cylinder(r1=4, r2=6, h=10);"
    example "cylinder(r=5, h=10, $fn = 6);"
    -- arguments
    r      :: ℝ    <- argument "r"
                `defaultTo` 1
                `doc` "radius of cylinder"
    h      :: Either ℝ ℝ2    <- argument "h"
                `defaultTo` Left 1
                `doc` "height of cylinder"
    r1     :: ℝ    <- argument "r1"
                `defaultTo` 1
                `doc` "bottom radius; overrides r"
    r2     :: ℝ    <- argument "r2"
                `defaultTo` 1
                `doc` "top radius; overrides r"
    sides  :: ℕ    <- argument "$fn"
                `defaultTo` (-1)
                `doc` "number of sides, for making prisms"
    center :: Bool <- argument "center"
                `defaultTo` False
                `doc` "center cylinder with respect to z?"
    -- Tests
    test "cylinder(r=10, h=30, center=true);"
        `eulerCharacteristic` 0
    test "cylinder(r=5, h=10, $fn = 6);"
        `eulerCharacteristic` 0
    let
        (h1, h2) = either (toInterval center) id h
        dh = h2 - h1
        shift :: SymbolicObj3 -> SymbolicObj3
        shift =
            if h1 == 0
            then id
            else Prim.translate (0,0,h1)
    -- The result is a computation state modifier that adds a 3D object,
    -- based on the args.
    addObj3 $ if r1 == 1 && r2 == 1
        then let
            obj2 = if sides < 0 then Prim.circle r else Prim.polygonR 0 $
                [(r*cos θ, r*sin θ )| θ <- [2*pi*(fromℕtoℝ n)/(fromℕtoℝ sides) | n <- [0 .. sides - 1]]]
            obj3 = Prim.extrudeR 0 obj2 dh
        in shift obj3
        else shift $ Prim.cylinder2 r1 r2 dh

circle :: (Symbol, [OVal] -> ArgParser (IO [OVal]))
circle = moduleWithoutSuite "circle" $ do
    example "circle(r=10); // circle"
    example "circle(r=5, $fn=6); //hexagon"
    -- Arguments
    r     :: ℝ <- argument "r"
               `doc` "radius of the circle"
    sides :: ℕ <- argument "$fn"
               `doc` "if defined, makes a regular polygon with n sides instead of a circle"
               `defaultTo` (-1)
    test "circle(r=10);"
        `eulerCharacteristic` 0
    addObj2 $ if sides < 3
        then Prim.circle r
        else Prim.polygonR 0 $
            [(r*cos θ, r*sin θ )| θ <- [2*pi*(fromℕtoℝ n)/(fromℕtoℝ sides) | n <- [0 .. sides - 1]]]

-- | FIXME: handle rectangles that are not grid alligned.
-- | FIXME: allow for rounding of polygon corners, specification of vertex ordering.
polygon :: (Symbol, [OVal] -> ArgParser (IO [OVal]))
polygon = moduleWithoutSuite "polygon" $ do
    example "polygon ([(0,0), (0,10), (10,0)]);"
    points :: [ℝ2]  <- argument "points"
                        `doc` "vertices of the polygon"
{-    r      :: ℝ     <- argument "r"
                        `doc` "rounding of the polygon corners"
    paths  :: [ℕ]   <- argument "paths"
                        `doc` "order to go through vertices"
                        `defaultTo` []
    case paths of
        [] -> addObj2 $ Prim.polygonR r points
        _ -> return $ return []
                        `defaultTo` 0
-}
    let
      addPolyOrSquare pts
        | [p1,p2,p3,p4] <- pts =
          let
            d1d2 = distanceSq p1 p2
            d3d4 = distanceSq p3 p4
            d1d3 = distanceSq p1 p3
            d2d4 = distanceSq p2 p4
            d1d4 = distanceSq p1 p4
            d2d3 = distanceSq p2 p3
            isGridAligned :: ℝ2 -> ℝ2 -> Bool
            isGridAligned (x1, y1) (x2, y2) = x1 == x2 || y1 == y2
          -- | Rectangles have no overlapping points,
          --   the distance on each side is equal to it's opposing side,
          --   and the distance between the pairs of opposing corners are equal.
          in if ((p1 /= p2 && p2 /= p3 && p3 /= p4 && p4 /= p1)
                 && (d1d2==d3d4 && d1d3==d2d4)
                 && (d1d4==d2d3))
             then if isGridAligned p1 p2
                  then Prim.rectR 0 p1 p3
                  else Prim.polygonR 0 pts
             else Prim.polygonR 0 pts
        | otherwise = Prim.polygonR 0 points
    addObj2 $ addPolyOrSquare points
union :: (Symbol, [OVal] -> ArgParser (IO [OVal]))
union = moduleWithSuite "union" $ \children -> do
    r :: ℝ <- argument "r"
        `defaultTo` 0
        `doc` "Radius of rounding for the union interface"
    return $ return $ if r > 0
        then objReduce (Prim.unionR r) (Prim.unionR r) children
        else objReduce  Prim.union      Prim.union     children

intersect :: (Symbol, [OVal] -> ArgParser (IO [OVal]))
intersect = moduleWithSuite "intersection" $ \children -> do
    r :: ℝ <- argument "r"
        `defaultTo` 0
        `doc` "Radius of rounding for the intersection interface"
    return $ return $ if r > 0
        then objReduce (Prim.intersectR r) (Prim.intersectR r) children
        else objReduce  Prim.intersect      Prim.intersect     children

difference :: (Symbol, [OVal] -> ArgParser (IO [OVal]))
difference = moduleWithSuite "difference" $ \children -> do
    r :: ℝ <- argument "r"
        `defaultTo` 0
        `doc` "Radius of rounding for the difference interface"
    return $ return $ if r > 0
        then objReduce (Prim.differenceR r) (Prim.differenceR r) children
        else objReduce  Prim.difference      Prim.difference     children

translate :: (Symbol, [OVal] -> ArgParser (IO [OVal]))
translate = moduleWithSuite "translate" $ \children -> do
    example "translate ([2,3]) circle (4);"
    example "translate ([5,6,7]) sphere(5);"
    (x,y,z) <-
        do
            x :: ℝ <- argument "x"
                `doc` "x amount to translate";
            y :: ℝ <- argument "y"
                `doc` "y amount to translate";
            z :: ℝ <- argument "z"
                `doc` "z amount to translate"
                `defaultTo` 0;
            return (x,y,z);
        <|> do
            v :: Either ℝ (Either ℝ2 ℝ3) <- argument "v"
                `doc` "vector to translate by"
            return $ case v of
                Left          x       -> (x,0,0)
                Right (Left  (x,y)  ) -> (x,y,0)
                Right (Right (x,y,z)) -> (x,y,z)
    return $ return $
        objMap (Prim.translate (x,y)) (Prim.translate (x,y,z)) children

deg2rad :: ℝ -> ℝ
deg2rad x = x / 180 * pi

-- This is mostly insane
-- | FIXME: rotating a module that is not found returns no geometry, instead of an error.
rotate :: (Symbol, [OVal] -> ArgParser (IO [OVal]))
rotate = moduleWithSuite "rotate" $ \children -> do
    a <- argument "a"
        `doc` "value to rotate by; angle or list of angles"
    v <- argument "v"
        `defaultTo` (0, 0, 1)
        `doc` "Vector to rotate around if a is a single angle"
    -- caseOType matches depending on whether size can be coerced into
    -- the right object. See Graphics.Implicit.ExtOpenScad.Util
    -- Entries must be joined with the operator <||>
    -- Final entry must be fall through.
    return $ return $ caseOType a $
               ( \θ  ->
                          objMap (Prim.rotate $ deg2rad θ) (Prim.rotate3V (deg2rad θ) v) children
        ) <||> ( \(yz,zx,xy) ->
            objMap (Prim.rotate $ deg2rad xy ) (Prim.rotate3 (deg2rad yz, deg2rad zx, deg2rad xy) ) children
        ) <||> ( \(yz,zx) ->
            objMap id (Prim.rotate3 (deg2rad yz, deg2rad zx, 0)) children
        ) <||> const []

scale :: (Symbol, [OVal] -> ArgParser (IO [OVal]))
scale = moduleWithSuite "scale" $ \children -> do
    example "scale(2) square(5);"
    example "scale([2,3]) square(5);"
    example "scale([2,3,4]) cube(5);"
    v <- argument "v"
        `doc` "vector or scalar to scale by"
    let
        scaleObjs stretch2 stretch3 =
            objMap (Prim.scale stretch2) (Prim.scale stretch3) children
    return $ return $ case v of
        Left   x              -> scaleObjs (x,1) (x,1,1)
        Right (Left (x,y))    -> scaleObjs (x,y) (x,y,1)
        Right (Right (x,y,z)) -> scaleObjs (x,y) (x,y,z)

extrude :: (Symbol, [OVal] -> ArgParser (IO [OVal]))
extrude = moduleWithSuite "linear_extrude" $ \children -> do
    example "linear_extrude(10) square(5);"
    height :: Either ℝ (ℝ -> ℝ -> ℝ) <- argument "height" `defaultTo` Left 1
        `doc` "height to extrude to..."
    center :: Bool <- argument "center" `defaultTo` False
        `doc` "center? (the z component)"
    twist  :: Maybe (Either ℝ (ℝ  -> ℝ)) <- argument "twist"  `defaultTo` Nothing
        `doc` "twist as we extrude, either a total amount to twist or a function..."
    scaleArg  :: Maybe (Either ℝ (ℝ  -> ℝ)) <- argument "scale"  `defaultTo` Nothing
        `doc` "scale according to this funciton as we extrude..."
    translateArg :: Maybe (Either ℝ2 (ℝ -> ℝ2)) <- argument "translate"  `defaultTo` Nothing
        `doc` "translate according to this funciton as we extrude..."
    r      :: ℝ   <- argument "r"      `defaultTo` 0
        `doc` "round the top?"
    let
        heightn = case height of
                Left  h -> h
                Right f -> f 0 0

        height' = case height of
            Right f -> Right $ uncurry f
            Left a -> Left a
        shiftAsNeeded :: SymbolicObj3 -> SymbolicObj3
        shiftAsNeeded =
            if center
            then Prim.translate (0,0,-heightn/2.0)
            else id
        funcify :: (VectorSpace a, s ~ (Scalar a), s ~ ℝ) => Either a (ℝ -> a) -> ℝ -> a
        funcify (Left val) h = (h/heightn) *^ val
        funcify (Right f ) h = f h
        twist' = fmap funcify twist
        scale' = fmap funcify scaleArg
        translate' = fmap funcify translateArg
    return $ return $ obj2UpMap (
        \obj -> case height of
            Left constHeight | isNothing twist && isNothing scaleArg && isNothing translateArg ->
                shiftAsNeeded $ Prim.extrudeR r obj constHeight
            _ ->
                shiftAsNeeded $ Prim.extrudeRM r twist' scale' translate' obj height'
        ) children

rotateExtrude :: (Symbol, [OVal] -> ArgParser (IO [OVal]))
rotateExtrude = moduleWithSuite "rotate_extrude" $ \children -> do
    example "rotate_extrude() translate(20) circle(10);"
    totalRot     :: ℝ <- argument "a" `defaultTo` 360
                    `doc` "angle to sweep"
    r            :: ℝ    <- argument "r"   `defaultTo` 0
    translateArg :: Either ℝ2 (ℝ -> ℝ2) <- argument "translate" `defaultTo` Left (0,0)
    rotateArg    :: Either ℝ  (ℝ -> ℝ ) <- argument "rotate" `defaultTo` Left 0
    let
        is360m :: ℝ -> Bool
        is360m n = 360 * fromInteger (round $ n / 360) /= n
        cap = is360m totalRot
            || either ( /= (0,0)) (\f -> f 0 /= f totalRot) translateArg
            || either is360m (\f -> is360m (f 0 - f totalRot)) rotateArg
        capM = if cap then Just r else Nothing
    return $ return $ obj2UpMap (Prim.rotateExtrude totalRot capM translateArg rotateArg) children

{-
rotateExtrudeStatement :: (Symbol, [OVal] -> ArgParser (IO [OVal]))
rotateExtrudeStatement = moduleWithSuite "rotate_extrude" $ \suite -> do
    -- arguments
    h :: ℝ <- realArgument "h"
    center :: Bool <- boolArgumentWithDefault "center" False
    twist :: ℝ <- realArgumentWithDefault 0
    r :: ℝ <- realArgumentWithDefault "r" 0

    getAndModUpObj2s suite (\obj -> extrudeRMod r (\θ (x,y) -> (x*cos(θ)+y*sin(θ), y*cos(θ)-x*sin(θ)) )  obj h)
-}

shell :: (Symbol, [OVal] -> ArgParser (IO [OVal]))
shell = moduleWithSuite "shell" $ \children -> do
    w :: ℝ <- argument "w"
            `doc` "width of the shell..."
    return $ return $ objMap (Prim.shell w) (Prim.shell w) children

-- Not a permanent solution! Breaks if can't pack.
pack :: (Symbol, [OVal] -> ArgParser (IO [OVal]))
pack = moduleWithSuite "pack" $ \children -> do
    example "pack ([45,45], sep=2) { circle(10); circle(10); circle(10); circle(10); }"
    -- arguments
    size :: ℝ2 <- argument "size"
        `doc` "size of 2D box to pack objects within"
    sep  :: ℝ  <- argument "sep"
        `doc` "mandetory space between objects"
    -- The actual work...
    return $
        let (obj2s, obj3s, others) = divideObjs children
        in if not $ null obj3s
            then case Prim.pack3 size sep obj3s of
                Just solution -> return $ OObj3 solution : (map OObj2 obj2s ++ others)
                Nothing       -> do
                    putStrLn "Can't pack given objects in given box with present algorithm"
                    return children
            else case Prim.pack2 size sep obj2s of
                Just solution -> return $ OObj2 solution : others
                Nothing       -> do
                    putStrLn "Can't pack given objects in given box with present algorithm"
                    return children

unit :: (Symbol, [OVal] -> ArgParser (IO [OVal]))
unit = moduleWithSuite "unit" $ \children -> do
    example "unit(\"inch\") {..}"
    -- arguments
    name :: String <- argument "unit"
        `doc` "the unit you wish to work in"
    let
        mmRatio :: String -> Maybe ℝ
        mmRatio "inch" = Just 25.4
        mmRatio "in"   = mmRatio "inch"
        mmRatio "foot" = Just 304.8
        mmRatio "ft"   = mmRatio "foot"
        mmRatio "yard" = Just 914.4
        mmRatio "yd"   = mmRatio "yard"
        mmRatio "mm"   = Just 1
        mmRatio "cm"   = Just 10
        mmRatio "dm"   = Just 100
        mmRatio "m"    = Just 1000
        mmRatio "km"   = Just 1000000
        mmRatio "µm"   = Just 0.001
        mmRatio "um"   = mmRatio "µm"
        mmRatio "nm"   = Just 0.0000001
        mmRatio _      = Nothing
    -- The actual work...
    return $ case mmRatio name of
        Nothing -> do
            putStrLn $ "unrecognized unit " ++ name
            return children
        Just r  ->
            return $ objMap (Prim.scale (r,r)) (Prim.scale (r,r,r)) children

---------------

(<|>) :: ArgParser a -> ArgParser a -> ArgParser a
(<|>) = mplus

moduleWithSuite :: String -> ([OVal] -> ArgParser (IO [OVal])) -> (Symbol, [OVal] -> ArgParser (IO [OVal]))
moduleWithSuite name modArgMapper = ((Symbol name), modArgMapper)
moduleWithoutSuite :: String -> ArgParser (IO [OVal]) -> (Symbol, b -> ArgParser (IO [OVal]))
moduleWithoutSuite name modArgMapper = ((Symbol name), const modArgMapper)

addObj2 :: SymbolicObj2 -> ArgParser (IO [OVal])
addObj2 x = return $ return [OObj2 x]

addObj3 :: SymbolicObj3 -> ArgParser (IO [OVal])
addObj3 x = return $ return [OObj3 x]

objMap :: (SymbolicObj2 -> SymbolicObj2) -> (SymbolicObj3 -> SymbolicObj3) -> [OVal] -> [OVal]
objMap obj2mod obj3mod (x:xs) = case x of
    OObj2 obj2 -> OObj2 (obj2mod obj2) : objMap obj2mod obj3mod xs
    OObj3 obj3 -> OObj3 (obj3mod obj3) : objMap obj2mod obj3mod xs
    a          -> a                    : objMap obj2mod obj3mod xs
objMap _ _ [] = []

objReduce :: ([SymbolicObj2] -> SymbolicObj2) -> ([SymbolicObj3] -> SymbolicObj3) -> [OVal] -> [OVal]
objReduce obj2reduce obj3reduce l = case divideObjs l of
    (   [],    [], others) ->                                                       others
    (   [], obj3s, others) ->                            OObj3 (obj3reduce obj3s) : others
    (obj2s,    [], others) -> OObj2 (obj2reduce obj2s)                            : others
    (obj2s, obj3s, others) -> OObj2 (obj2reduce obj2s) : OObj3 (obj3reduce obj3s) : others

obj2UpMap :: (SymbolicObj2 -> SymbolicObj3) -> [OVal] -> [OVal]
obj2UpMap obj2upmod (x:xs) = case x of
    OObj2 obj2 -> OObj3 (obj2upmod obj2) : obj2UpMap obj2upmod xs
    a          -> a                      : obj2UpMap obj2upmod xs
obj2UpMap _ [] = []

toInterval :: Bool -> ℝ -> ℝ2
toInterval center h =
    if center
    then (-h/2, h/2)
    else (0, h)
