-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright 2016, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

module Graphics.Implicit.ObjectUtil.GetBox2 (getBox2, getBox2R) where

import Prelude(Bool, Fractional, (==), (||), unzip, minimum, maximum, ($), filter, not, (.), (/), map, (-), (+), (*), cos, sin, sqrt, min, max, abs, head, (<), fst, snd, (++), pi, atan2, (>=), (==), (>), (&&))

import Graphics.Implicit.Definitions (ℝ, ℝ2, Box2, (⋯*),
                                      SymbolicObj2(Shell2, Outset2, Circle, Translate2, Rotate2, UnionR2, Scale2, RectR,
                                                   PolygonR, Complement2, DifferenceR2, IntersectR2, EmbedBoxedObj2))

import Data.VectorSpace (magnitude, (^-^), (^+^))

import Data.Fixed (mod')

-- | Is a Box2 empty?
-- | Really, this checks if it is one dimensional, which is good enough.
isEmpty :: Box2 -> Bool
isEmpty ((a, b), (c, d)) = a==c || b==d

-- | Define a Box2 around all of the given points.
pointsBox :: [ℝ2] -> Box2
pointsBox points =
    let
        (xs, ys) = unzip points
    in
        ((minimum xs, minimum ys), (maximum xs, maximum ys))

-- | Define a box that fits around the given boxes.
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
getBox2 (Circle r) = ((-r, -r), (r,r))
getBox2 (PolygonR _ points) = pointsBox points
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
        (sax, say) = s ⋯* a
        (sbx, sby) = s ⋯* b
    in
        ((min sax sbx, min say sby), (max sax sbx, max say sby))
getBox2 (Rotate2 θ symbObj) =
    let
        ((x1,y1), (x2,y2)) = getBox2 symbObj
        rotate (x,y) = (x*cos θ - y*sin θ, x*sin θ + y*cos θ)
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

-- Define a Box2 around the given object, and the space it occupies while rotating.
getBox2R :: SymbolicObj2 -> ℝ -> Box2
-- FIXME: more implementations.
--getBox2R (RectR _ a b) deg = (a,b)
getBox2R (Circle r) _ = ((-r, -r),(r,r))
getBox2R (PolygonR r points) deg =
  let
    pointRBoxes = [ pointRBox point deg | point <- points ]
    pointValsMin = map fst pointRBoxes
    pointValsMax = map snd pointRBoxes
    (pointValsX, pointValsY) = unzip (pointValsMin ++ pointValsMax)
  in
    ((r + (minimum pointValsX), r + (minimum pointValsY)),(r + (maximum pointValsX), r + (maximum pointValsY)))
-- Fallthrough: use getDist2 to overestimate.
getBox2R symbObj _ = ((-d, -d), (d, d))
  where
    d = getDist2 (0,0) symbObj

-- | put a box around a point, and all of the locations it's been during an x degree arc around (0,0).
pointRBox :: ℝ2 -> ℝ -> Box2
pointRBox (x,y) arc =
  let
    k :: ℝ
    k = pi/180
    -- our target, radian-style.
    distance = sqrt $ x*x + y*y
    θstart = atan2 y x
    rot = arc * k
    θstop = θstart + rot
    (xStop, yStop) = (distance*cos(θstop), distance*sin(θstop))
    (minX, minY, maxX, maxY) = (min x xStop, min y yStop, max x xStop, max y yStop)
    absrad :: ℝ -> ℝ
    absrad rad =
      if rad >= 0
      then rad `mod'` (360*k)
      else -((abs rad) `mod'` (360*k)) + (360*k)
    crossOne rad = ((min minX crossX, min minY crossY), (max maxX crossX, max maxY crossY))
      where
        (crossX, crossY) = case absrad rad of
                             radians | radians <  (90*k) -> (distance,0)
                                     | radians < (180*k) -> (0, -distance)
                                     | radians < (270*k) -> (-distance,0)
                             _                           -> (0, distance)
    crossTwo rad = ((min minX crossMinX, min minY crossMinY), (max maxX crossMaxX, max maxY crossMaxY))
      where
        (crossMinX, crossMinY, crossMaxX, crossMaxY) = case absrad rad of
                                                         radians | radians <  (90*k) -> (0, -distance,  distance, 0)
                                                                 | radians < (180*k) -> (-distance, -distance, 0, 0)
                                                                 | radians < (270*k) -> (-distance, 0,  0, distance)
                                                         _                           -> (0, 0,  distance,  distance)
    crossThree rad = ((min minX crossMinX, min minY crossMinY), (max maxX crossMaxX, max maxY crossMaxY))
      where
        (crossMinX, crossMinY, crossMaxX, crossMaxY) = case absrad rad of
                                                         radians | radians <  (90*k) -> (-distance, -distance,  distance, 0)
                                                                 | radians < (180*k) -> (-distance, -distance, 0,  distance)
                                                                 | radians < (270*k) -> (-distance, 0,  distance,  distance)
                                                         _                           -> (0, -distance,  distance,  distance)
  in
    -- FIXME: more cases. cover negative rotation.
    case arc of
      degrees | degrees <  90 && arc > 0 ->
                  if θstop `mod'` 90*k == θstop
                  then ((minX, minY), (maxX, maxY))
                  else crossOne θstart
              | degrees < 180 && arc > 0 ->
                  if θstop `mod'` 180*k == θstop `mod'` 90*k
                  then crossOne θstart
                  else crossTwo θstart
              | degrees < 270 && arc > 0 ->
                  if θstop `mod'` 270*k == θstop `mod'` 180*k
                  then crossTwo θstart
                  else crossThree θstart
              | degrees < 360 && arc > 0 ->
                  if θstop `mod'` 360*k == θstop `mod'` 270*k
                  then crossThree θstart
                  else ((-distance, -distance), (distance, distance))
      _                                  ->
                  ((-distance, -distance), (distance, distance))

-- Get the maximum distance (read upper bound) an object is from a point.
-- Sort of a circular
getDist2 :: ℝ2 -> SymbolicObj2 -> ℝ
-- Real implementations
getDist2 p (Circle r) =  magnitude p + r
getDist2 p (PolygonR r points) = r + maximum [magnitude (p ^-^ p') | p' <- points]
-- Transform implementations
getDist2 p (UnionR2 r objs) = r + maximum [getDist2 p obj | obj <- objs ]
getDist2 p (DifferenceR2 r objs) = r + getDist2 p (head objs)
getDist2 p (IntersectR2 r objs) = r + maximum [getDist2 p obj | obj <- objs ]
-- FIXME: isn't this wrong? should we be returning distance inside of the object?
getDist2 _ (Complement2 _) = 1/0
getDist2 p (Translate2 v obj) = getDist2 (p ^+^ v) obj
-- FIXME: write optimized functions for the rest of the SymbObjs.
-- Fallthrough: use getBox2 to check the distance a box is from the point.
getDist2 (x,y) symbObj =
    let
        ((x1,y1), (x2,y2)) = getBox2 symbObj
    in
        sqrt (
              max (abs (x1 - x)) (abs (x2 - x)) *
              max (abs (x1 - x)) (abs (x2 - x)) +
              max (abs (y1 - y)) (abs (y2 - y)) *
              max (abs (y1 - y)) (abs (y2 - y))
             )

