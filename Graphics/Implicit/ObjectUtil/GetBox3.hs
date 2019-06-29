-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright 2014 2015 2016, Julia Longtin (julial@turinglace.com)
-- Copyright 2015 2016, Mike MacHenry (mike.machenry@gmail.com)
-- Released under the GNU AGPLV3+, see LICENSE

-- FIXME: required. why?
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, TypeSynonymInstances, UndecidableInstances #-}

module Graphics.Implicit.ObjectUtil.GetBox3 (getBox3) where

import Prelude(Eq, Bool(False), Either (Left, Right), (==), (||), max, (/), (-), (+), map, unzip, ($), filter, not, (.), unzip3, minimum, maximum, min, (>), (&&), head, (*), (<), abs, either, error, const, otherwise, take)

import Data.Maybe(Maybe(Nothing, Just))

import Graphics.Implicit.Definitions (ℝ, Fastℕ, Box3, SymbolicObj3 (Rect3R, Sphere, Cylinder, Complement3, UnionR3, IntersectR3, DifferenceR3, Translate3, Scale3, Rotate3, Rotate3V, Shell3, Outset3, EmbedBoxedObj3, ExtrudeR, ExtrudeOnEdgeOf, ExtrudeRM, RotateExtrude, ExtrudeRotateR), SymbolicObj2 (Rotate2, RectR), (⋯*), infty, neginfty, fromFastℕtoℝ, fromFastℕ)

import Graphics.Implicit.ObjectUtil.GetBox2 (getBox2, getDist2)

import Data.Maybe (fromMaybe)
import Data.VectorSpace ((^-^), (^+^))

-- FIXME: many variables are being ignored here. no rounding for intersect, or difference.. etc.

-- Test to see whether a Box3 has area.
isEmpty :: (Eq a2, Eq a1, Eq a) =>
           ((a, a1, a2), (a, a1, a2)) -> Bool
isEmpty ((a,b,c),(d,e,f)) = a==d || b==e || c==f

outsetBox :: ℝ -> Box3 -> Box3
outsetBox r (a,b) =
    (a ^-^ (r,r,r), b ^+^ (r,r,r))

-- Get a Box3 around the given object.
getBox3 :: SymbolicObj3 -> Box3
-- Primitives
getBox3 (Rect3R _ a b) = (a,b)
getBox3 (Sphere r) = ((-r, -r, -r), (r,r,r))
getBox3 (Cylinder h r1 r2) = ( (-r,-r,0), (r,r,h) ) where r = max r1 r2
-- (Rounded) CSG
getBox3 (Complement3 _) =
    ((neginfty, neginfty, neginfty), (infty, infty, infty))
getBox3 (UnionR3 r symbObjs) = ((left-r,bot-r,inward-r), (right+r,top+r,out+r))
    where
        boxes = map getBox3 symbObjs
        (leftbot, topright) = unzip $ filter (not.isEmpty) boxes
        (lefts, bots, ins) = unzip3 leftbot
        (rights, tops, outs) = unzip3 topright
        left = minimum lefts
        bot = minimum bots
        inward = minimum ins
        right = maximum rights
        top = maximum tops
        out = maximum outs
getBox3 (IntersectR3 _ symbObjs) =
    let
        boxes = map getBox3 symbObjs
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
        else ((0,0,0),(0,0,0))
getBox3 (DifferenceR3 _ symbObjs)  = getBox3 $ head symbObjs
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
        rotate v1 w1 v2 w2 angle = getBox2(Rotate2 angle $ RectR 0 (v1, w1) (v2, w2))
        ((y1', z1'), (y2', z2')) = rotate y1 z1 y2 z2 a
        ((z1'', x1'), (z2'', x2')) = rotate z1' x1 z2' x2 b
        ((x1'', y1''), (x2'', y2'')) = rotate x1' y1' x2' y2' c
        (xs, ys, zs) = ([x1'', x2''], [y1'', y2''], [z1'', z2''])
    in
        ((minimum xs, minimum ys, minimum zs), (maximum xs, maximum ys, maximum zs))

getBox3 (Rotate3V _ v symbObj) = getBox3 (Rotate3 v symbObj)
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
-- FIXME: magic numbers.
getBox3 (ExtrudeRM _ twist scale translate symbObj eitherh) =
    let
        samples :: Fastℕ
        samples=11
        hfuzz :: ℝ
        hfuzz = 0.2
        range :: [Fastℕ]
        range = [0, 1 .. (samples-1)]
        ((x1,y1),(x2,y2)) = getBox2 symbObj
        (dx,dy) = (x2 - x1, y2 - y1)
        (xrange, yrange) = ( map (\s -> x1+s*dx/(fromFastℕtoℝ $ samples-1)) $ map fromFastℕtoℝ range, map (\s -> y1+s*dy/(fromFastℕtoℝ $ samples-1)) $ map fromFastℕtoℝ range )
        h = case eitherh of
              Left h' -> h'
              Right hf -> hmax + hfuzz*(hmax-hmin)
                where
                    hs = [hf (x,y) | x <- xrange, y <- yrange]
                    (hmin, hmax) = (minimum hs, maximum hs)
        hrange = map (/(fromFastℕtoℝ $ samples-1)) $ map (h*) $ map fromFastℕtoℝ range
        sval = case scale of
            Nothing -> 1
            Just scale' -> maximum $ map (abs . scale') hrange
        (twistXmin, twistYmin, twistXmax, twistYmax) = case twist of
            Nothing -> (smin x1, smin y1, smax x2, smax y2)
                where
                    smin y = min y (sval * y)
                    smax y = max y (sval * y)
            Just _  -> (-d, -d, d, d)
                where d = sval * getDist2 (0,0) symbObj
        translate' = fromMaybe (const (0,0)) translate
        (tvalsx, tvalsy) = unzip $ map (translate' . (h*)) hrange
        (tminx, tminy) = (minimum tvalsx, minimum tvalsy)
        (tmaxx, tmaxy) = (maximum tvalsx, maximum tvalsy)
    in
        ((twistXmin + tminx, twistYmin + tminy, 0),(twistXmax + tmaxx, twistYmax + tmaxy, h))
-- Note: Assumes x2 is always greater than x1.
-- FIXME: Insert the above assumption as an assertion in the language structure?
getBox3 (RotateExtrude _ _ (Left (xshift,yshift)) _ symbObj) =
    let
        ((_,y1),(x2,y2)) = getBox2 symbObj
        r = max x2 (x2 + xshift)
    in
        ((-r, -r, min y1 (y1 + yshift)),(r, r, max y2 (y2 + yshift)))
-- FIXME: magic numbers.
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
        step = rot/(fromFastℕtoℝ $ samples-1)
        ((x1,y1),(x2,y2)) = getBox2 symbObj
        (xrange, yrange) = unzip $ take (fromFastℕ samples) $ map f $ map (step*) $ map fromFastℕtoℝ range
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
                s = maximum $ map abs [x2, y1, y2]
            in (s + xmax', s + ymin', y2 + ymax')
            else (x2 + xmax', y1 + ymin', y2 + ymax')
    in
        ((-r, -r, y1 + ymin'),(r, r, y2 + ymax'))
-- FIXME: add case for ExtrudeRotateR!
getBox3 ExtrudeRotateR{} = error "ExtrudeRotateR implementation incomplete!"
