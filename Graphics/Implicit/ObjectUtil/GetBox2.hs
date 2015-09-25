-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts, TypeSynonymInstances, UndecidableInstances #-}

module Graphics.Implicit.ObjectUtil.GetBox2 (getBox2, getDist2) where

import Graphics.Implicit.Definitions
import Data.VectorSpace

isEmpty :: Box2 -> Bool
isEmpty = (== ((0,0), (0,0)))

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

getBox2 :: SymbolicObj2 -> Box2

-- Primitives
getBox2 (RectR _ a b) = (a,b)

getBox2 (Circle r ) =  ((-r, -r), (r,r))

getBox2 (PolygonR _ points) = ((minimum xs, minimum ys), (maximum xs, maximum ys))
     where (xs, ys) = unzip points

-- (Rounded) CSG
getBox2 (Complement2 _) =
    ((-infty, -infty), (infty, infty)) where infty = 1/0

getBox2 (UnionR2 r symbObjs) =
    outsetBox r $ unionBoxes (map getBox2 symbObjs)

getBox2 (DifferenceR2 _ symbObjs) =
    let 
        firstBox:_ = map getBox2 symbObjs
    in
        firstBox

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
    in
        (s ⋯* a, s ⋯* b)

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

getDist2 (x,y) symbObj =
    let
        ((x1,y1), (x2,y2)) = getBox2 symbObj
    in
        sqrt ((max (abs (x1 - x)) (abs (x2 - x)))**2 + (max (abs (y1 - y)) (abs (y2 - y)))**2)

