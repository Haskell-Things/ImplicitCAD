-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright 2014 2015 2016, Julia Longtin (julial@turinglace.com)
-- Copyright 2015 2016, Mike MacHenry (mike.machenry@gmail.com)
-- Released under the GNU AGPLV3+, see LICENSE

module Graphics.Implicit.ObjectUtil.GetBox3 (getBox3) where

import Prelude(Eq, Bool(False), Fractional, Either (Left, Right), (==), (||), max, (/), (-), (+), fmap, unzip, ($), (<$>), filter, not, (.), unzip3, minimum, maximum, min, (>), (&&), (*), (<), abs, either, error, const, otherwise, take, fst, snd)

import Graphics.Implicit.Definitions (ℝ3, ℝ, Fastℕ, Box3, SymbolicObj3 (CubeR, Sphere, Cylinder, Complement3, UnionR3, IntersectR3, DifferenceR3, Translate3, Scale3, Rotate3, Rotate3V, Mirror3, Shell3, Outset3, EmbedBoxedObj3, ExtrudeR, ExtrudeOnEdgeOf, ExtrudeRM, RotateExtrude, ExtrudeRotateR), SymbolicObj2 (Rotate2, SquareR, Translate2), ExtrudeRMScale(C1, C2), (⋯*), fromFastℕtoℝ, fromFastℕ, toScaleFn)

import Graphics.Implicit.ObjectUtil.GetBox2 (getBox2, getBox2R)

import Data.VectorSpace ((^-^), (^+^))
import Graphics.Implicit.MathUtil (reflect)

-- FIXME: many variables are being ignored here. no rounding for intersect, or difference.. etc.

-- | An empty box.
emptyBox :: Box3
emptyBox = ((0,0,0), (0,0,0))

-- | Define a Box3 around all of the given points.
pointsBox :: [ℝ3] -> Box3
pointsBox [] = emptyBox
pointsBox points =
    let
        (xs, ys, zs) = unzip3 points
    in
        ((minimum xs, minimum ys, minimum zs), (maximum xs, maximum ys, maximum zs))

-- | Is a Box3 empty?
-- | Really, this checks if it is one dimensional, which is good enough.
isEmpty :: (Eq a2, Eq a1, Eq a) =>
           ((a, a1, a2), (a, a1, a2)) -> Bool
isEmpty ((a,b,c),(d,e,f)) = a==d || b==e || c==f

-- | Increase a boxes size by a rounding value.
outsetBox :: ℝ -> Box3 -> Box3
outsetBox r (a,b) =
    (a ^-^ (r,r,r), b ^+^ (r,r,r))

-- Get a Box3 around the given object.
getBox3 :: SymbolicObj3 -> Box3
-- Primitives
getBox3 (CubeR _ size) = ((0, 0, 0), size)
getBox3 (Sphere r) = ((-r, -r, -r), (r,r,r))
getBox3 (Cylinder h r1 r2) = ( (-r,-r,0), (r,r,h) ) where r = max r1 r2
-- (Rounded) CSG
getBox3 (Complement3 _) =
    ((-infty, -infty, -infty), (infty, infty, infty))
        where
          infty :: (Fractional t) => t
          infty = 1/0
getBox3 (UnionR3 r symbObjs) = outsetBox r ((left,bot,inward), (right,top,out))
    where
        boxes = fmap getBox3 symbObjs
        (leftbot, topright) = unzip $ filter (not.isEmpty) boxes
        (lefts, bots, ins) = unzip3 leftbot
        (rights, tops, outs) = unzip3 topright
        left = minimum lefts
        bot = minimum bots
        inward = minimum ins
        right = maximum rights
        top = maximum tops
        out = maximum outs
getBox3 (DifferenceR3 _ symbObj _)  = getBox3 symbObj
getBox3 (IntersectR3 _ symbObjs) =
    let
        boxes = fmap getBox3 symbObjs
        (leftbot, topright) = unzip boxes
        (lefts, bots, ins) = unzip3 leftbot
        (rights, tops, outs) = unzip3 topright
        left = maximum lefts
        bot = maximum bots
        inward = maximum ins
        right = minimum rights
        top = minimum tops
        out = minimum outs
    in
        if   top   > bot
          && right > left
          && out   > inward
        then ((left,bot,inward),(right,top,out))
        else emptyBox
-- Simple transforms
getBox3 (Translate3 v symbObj) =
    let
        (a,b) = getBox3 symbObj
    in
        (a^+^v, b^+^v)
getBox3 (Scale3 s symbObj) =
    let
        (a,b) = getBox3 symbObj
        (sax,say,saz) = s ⋯* a
        (sbx,sby,sbz) = s ⋯* b
    in
        ((min sax sbx, min say sby, min saz sbz), (max sax sbx, max say sby, max saz sbz))
getBox3 (Rotate3 (a, b, c) symbObj) =
    let
        ((x1, y1, z1), (x2, y2, z2)) = getBox3 symbObj
        rotate v1 w1 v2 w2 angle = getBox2(Rotate2 angle $ Translate2 (v1, w1) $ SquareR 0 (v2-v1, w2-w1))
        ((y1', z1'), (y2', z2')) = rotate y1 z1 y2 z2 a
        ((z1'', x1'), (z2'', x2')) = rotate z1' x1 z2' x2 b
        ((x1'', y1''), (x2'', y2'')) = rotate x1' y1' x2' y2' c
        (xs, ys, zs) = ([x1'', x2''], [y1'', y2''], [z1'', z2''])
    in
        ((minimum xs, minimum ys, minimum zs), (maximum xs, maximum ys, maximum zs))

getBox3 (Rotate3V _ v symbObj) = getBox3 (Rotate3 v symbObj)
getBox3 (Mirror3 v symbObj) =
    let (p1@(x1, y1, z1), p2@(x2, y2, z2)) = getBox3 symbObj
     in pointsBox
          [ reflect v p1
          , reflect v (x1, y2, z1)
          , reflect v (x2, y2, z1)
          , reflect v (x2, y1, z1)
          , reflect v (x1, y1, z2)
          , reflect v (x2, y1, z2)
          , reflect v (x1, y2, z2)
          , reflect v p2
          ]
-- Boundary mods
getBox3 (Shell3 w symbObj) =
    outsetBox (w/2) $ getBox3 symbObj
getBox3 (Outset3 d symbObj) =
    outsetBox d $ getBox3 symbObj
-- Misc
getBox3 (EmbedBoxedObj3 (_,box)) = box
-- 2D Based
getBox3 (ExtrudeR _ symbObj h) = ((x1,y1,0),(x2,y2,h))
    where
        ((x1,y1),(x2,y2)) = getBox2 symbObj
getBox3 (ExtrudeOnEdgeOf symbObj1 symbObj2) =
    let
        ((ax1,ay1),(ax2,ay2)) = getBox2 symbObj1
        ((bx1,by1),(bx2,by2)) = getBox2 symbObj2
    in
        ((bx1+ax1, by1+ax1, ay1), (bx2+ax2, by2+ax2, ay2))
-- FIXME: magic numbers: 0.2 and 11.
-- FIXME: this may use an approximation, based on sampling functions. generate a warning if the approximation part of this function is used.
-- FIXME: re-implement the expression system, so this can recieve a function, and determine class (constant, linear)... and implement better forms of this function.
getBox3 (ExtrudeRM _ twist scale translate symbObj height) =
    let
        ((x1,y1),(x2,y2)) = getBox2 symbObj
        (dx,dy) = (x2 - x1, y2 - y1)

        samples :: Fastℕ
        samples = 11
        range :: [Fastℕ]
        range = [0, 1 .. (samples-1)]
        (xrange, yrange) = ( fmap ((\s -> x1+s*dx/fromFastℕtoℝ (samples-1)) . fromFastℕtoℝ) range, fmap ((\s -> y1+s*dy/fromFastℕtoℝ (samples-1)) . fromFastℕtoℝ) range)

        hfuzz :: ℝ
        hfuzz = 0.2
        h = case height of
              Left hval -> hval
              Right hfun -> hmax + hfuzz*(hmax-hmin)
                where
                    hs = [hfun (x,y) | x <- xrange, y <- yrange]
                    (hmin, hmax) = (minimum hs, maximum hs)
        hrange = fmap ((/ fromFastℕtoℝ (samples-1)) . (h*) . fromFastℕtoℝ) range

        (twistXmin, twistYmin, twistXmax, twistYmax) =
          let
            both f (a, b) = (f a, f b)
            (scalex', scaley') = case scale of
              C1 s -> (s, s)
              C2 s -> s
              s -> both maximum . unzip $ both abs . toScaleFn s <$> hrange
            smin s v = min v (s * v)
            smax s v = max v (s * v)
            -- FIXME: assumes minimums are negative, and maximums are positive.
            scaleEach ((d1, d2),(d3, d4)) = (scalex' * d1, scaley' * d2, scalex' * d3, scaley' * d4)
          in case twist of
            Left twval -> if twval == 0
                          then (smin scalex' x1, smin scaley' y1, smax scalex' x2, smax scaley' y2)
                          else scaleEach $ getBox2R symbObj twval
            Right _  -> scaleEach $ getBox2R symbObj 360 -- we can't range functions yet, so assume a full circle.

        (tminx, tmaxx, tminy, tmaxy) =
          let
            tvalsx :: (ℝ -> (ℝ, ℝ)) -> [ℝ]
            tvalsx tfun = fmap (fst . tfun) hrange
            tvalsy :: (ℝ -> (ℝ, ℝ)) -> [ℝ]
            tvalsy tfun = fmap (snd . tfun) hrange
          in case translate of
            Left  (tvalx, tvaly) -> (tvalx, tvalx, tvaly, tvaly)
            Right tfun -> (minimum $ tvalsx tfun, maximum $ tvalsx tfun, minimum $ tvalsy tfun, maximum $ tvalsy tfun)
    in
        ((twistXmin + tminx, twistYmin + tminy, 0),(twistXmax + tmaxx, twistYmax + tmaxy, h))
-- Note: Assumes x2 is always greater than x1.
-- FIXME: Insert the above assumption as an assertion in the type system?
getBox3 (RotateExtrude _ _ (Left (xshift,yshift)) _ symbObj) =
    let
        ((_,y1),(x2,y2)) = getBox2 symbObj
        r = max x2 (x2 + xshift)
    in
        ((-r, -r, min y1 (y1 + yshift)),(r, r, max y2 (y2 + yshift)))
-- FIXME: magic numbers: 0.1, 1.1, and 11.
-- FIXME: this may use an approximation, based on sampling functions. generate a warning if the approximation part of this function is used.
-- FIXME: re-implement the expression system, so this can recieve a function, and determine class (constant, linear)... and implement better forms of this function.
getBox3 (RotateExtrude rot _ (Right f) rotate symbObj) =
    let
        samples :: Fastℕ
        samples = 11
        xfuzz :: ℝ
        xfuzz = 1.1
        yfuzz :: ℝ
        yfuzz=0.1
        range :: [Fastℕ]
        range = [0, 1 .. (samples-1)]
        step = rot/fromFastℕtoℝ (samples-1)
        ((x1,y1),(x2,y2)) = getBox2 symbObj
        (xrange, yrange) = unzip $ take (fromFastℕ samples) $ fmap (f . (step*) . fromFastℕtoℝ) range
        xmax = maximum xrange
        ymax = maximum yrange
        ymin = minimum yrange
        xmax' | xmax > 0 = xmax * xfuzz
              | xmax < - x1 = 0
              | otherwise = xmax
        ymax' = ymax + yfuzz * (ymax - ymin)
        ymin' = ymin - yfuzz * (ymax - ymin)
        (r, _, _) = if either (==0) (const False) rotate
            then let
                s = maximum $ fmap abs [x2, y1, y2]
            in (s + xmax', s + ymin', y2 + ymax')
            else (x2 + xmax', y1 + ymin', y2 + ymax')
    in
        ((-r, -r, y1 + ymin'),(r, r, y2 + ymax'))
-- FIXME: add case for ExtrudeRotateR!
getBox3 ExtrudeRotateR{} = error "ExtrudeRotateR implementation incomplete!"
