-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright 2016, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts, TypeSynonymInstances, UndecidableInstances #-}

module Graphics.Implicit.ObjectUtil.GetBox2 (getBox2, getDist2) where

import Prelude(Bool, Fractional, (==), (||), unzip, minimum, maximum, ($), filter, not, (.), (/), map, (-), (+), (*), cos, sin, sqrt, min, max, abs, head)

import Graphics.Implicit.Definitions (ℝ, ℝ2, Box2, (⋯*),
                                      SymbolicObj2(Shell2, Outset2, Circle, Translate2, Rotate2, UnionR2, Scale2, RectR,
                                                   PolygonR, Complement2, DifferenceR2, IntersectR2, EmbedBoxedObj2))

import Data.VectorSpace (magnitude, (^-^), (^+^))

-- Is a Box2 empty?
-- Really, this checks if it is one dimensional, which is good enough.
isEmpty :: Box2 -> Bool
isEmpty ((a, b), (c, d)) = a==c || b==d

-- Define a Box2 around all of the given points.
pointsBox :: [ℝ2] -> Box2
pointsBox points =
    let
        (xs, ys) = unzip points
    in
        ((minimum xs, minimum ys), (maximum xs, maximum ys))

unionBoxes :: [Box2] -> Box2
unionBoxes boxes =
    let
        (leftbot, topright) = unzip $ filter (not.isEmpty) boxes
        (lefts, bots) = unzip leftbot
        (rights, tops) = unzip topright
    in
        ((minimum lefts, minimum bots), (maximum rights, maximum tops))

outsetBox :: ℝ -> Box2 -> Box2
outsetBox r (a,b) =
        (a ^-^ (r,r), b ^+^ (r,r))

-- Define a Box2 around the given object.
getBox2 :: SymbolicObj2 -> Box2
-- Primitives
getBox2 (RectR _ a b) = (a,b)
getBox2 (Circle r ) =  ((-r, -r), (r,r))
getBox2 (PolygonR _ points) = ((minimum xs, minimum ys), (maximum xs, maximum ys))
     where (xs, ys) = unzip points
-- (Rounded) CSG
getBox2 (Complement2 _) =
    ((-infty, -infty), (infty, infty))
        where
          infty :: (Fractional t) => t
          infty = 1/0
getBox2 (UnionR2 r symbObjs) =
    outsetBox r $ unionBoxes (map getBox2 symbObjs)
getBox2 (DifferenceR2 _ symbObjs) = getBox2 $ head symbObjs
getBox2 (IntersectR2 r symbObjs) =
    let
        boxes = map getBox2 symbObjs
        (leftbot, topright) = unzip boxes
        (lefts, bots) = unzip leftbot
        (rights, tops) = unzip topright
        left = maximum lefts
        bot = maximum bots
        right = minimum rights
        top = minimum tops
    in
        ((left-r,bot-r),(right+r,top+r))
-- Simple transforms
getBox2 (Translate2 v symbObj) =
    let
        (a,b) = getBox2 symbObj
    in
        if isEmpty (a,b)
        then ((0,0),(0,0))
        else (a^+^v, b^+^v)
getBox2 (Scale2 s symbObj) =
  let
        (a,b) = getBox2 symbObj
        ((x_a,y_a),(x_b,y_b)) = (s ⋯* a, s ⋯* b)
        a' = (min x_a x_b, min y_a y_b)
        b' = (max x_a x_b, max y_a y_b)
  in
        (a',b')

getBox2 (Rotate2 θ symbObj) =
  let
        ((x1,y1), (x2,y2)) = getBox2 symbObj
        rotate (x,y) = (cos(θ)*x - sin(θ)*y, sin(θ)*x + cos(θ)*y)
  in
        pointsBox [ rotate (x1, y1)
                  , rotate (x1, y2)
                  , rotate (x2, y1)
                  , rotate (x2, y2)
                  ]

-- Boundary mods
getBox2 (Shell2 w symbObj) =
    outsetBox (w/2) $ getBox2 symbObj
getBox2 (Outset2 d symbObj) =
    outsetBox d $ getBox2 symbObj
-- Misc
getBox2 (EmbedBoxedObj2 (_,box)) = box

-- Get the maximum distance (read upper bound) an object is from a point.
-- Sort of a circular
getDist2 :: ℝ2 -> SymbolicObj2 -> ℝ
getDist2 p (UnionR2 r objs) = r + maximum [getDist2 p obj | obj <- objs ]
getDist2 p (Translate2 v obj) = getDist2 (p ^+^ v) obj
getDist2 p (Circle r) = magnitude p + r
getDist2 p (PolygonR r points) =
    r + maximum [magnitude (p ^-^ p') | p' <- points]
-- FIXME: write optimized functions for the rest of the SymbObjs.
getDist2 (x,y) symbObj =
    let
        ((x1,y1), (x2,y2)) = getBox2 symbObj
    in
        sqrt (
              (max (abs (x1 - x)) (abs (x2 - x))) *
              (max (abs (x1 - x)) (abs (x2 - x))) +
              (max (abs (y1 - y)) (abs (y2 - y))) *
              (max (abs (y1 - y)) (abs (y2 - y)))
             )

