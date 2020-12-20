-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright 2014 2015 2016, Julia Longtin (julial@turinglace.com)
-- Copyright 2015 2016, Mike MacHenry (mike.machenry@gmail.com)
-- Released under the GNU AGPLV3+, see LICENSE

module Graphics.Implicit.ObjectUtil.GetBox3 (getBox3) where

import Prelude(uncurry, pure, Bool(False), Either (Left, Right), (==), max, (/), (-), (+), fmap, unzip, ($), (<$>), (.), minimum, maximum, min, (>), (*), (<), abs, either, error, const, otherwise, take, fst, snd)

import Graphics.Implicit.Definitions
    ( Fastℕ,
      fromFastℕ,
      ExtrudeRMScale(C2, C1),
      SymbolicObj3(Shared3, Cube, Sphere, Cylinder, Rotate3, Extrude, ExtrudeOnEdgeOf, ExtrudeM, RotateExtrude, ExtrudeRotateR),
      Box3,
      ℝ,
      fromFastℕtoℝ,
      toScaleFn )

import Graphics.Implicit.ObjectUtil.GetBox2 (getBox2, getBox2R)

import qualified Linear.Quaternion as Q
import Graphics.Implicit.ObjectUtil.GetBoxShared (corners, pointsBox, getBoxShared)

import Linear (V2(V2), V3(V3))

-- FIXME: many variables are being ignored here. no rounding for intersect, or difference.. etc.

-- Get a Box3 around the given object.
getBox3 :: SymbolicObj3 -> Box3
-- Primitives
getBox3 (Shared3 obj) = getBoxShared obj
getBox3 (Cube size) = (pure 0, size)
getBox3 (Sphere r) = (pure (-r), pure r)
getBox3 (Cylinder h r1 r2) = (V3 (-r) (-r) 0, V3 r r h ) where r = max r1 r2
-- (Rounded) CSG
-- Simple transforms
getBox3 (Rotate3 q symbObj) =
    let box = getBox3 symbObj
     in pointsBox $ Q.rotate q <$> corners box
-- Misc
-- 2D Based
getBox3 (Extrude symbObj h) = (V3 x1 y1 0, V3 x2 y2 h)
    where
        (V2 x1 y1, V2 x2 y2) = getBox2 symbObj
getBox3 (ExtrudeOnEdgeOf symbObj1 symbObj2) =
    let
        (V2 ax1 ay1, V2 ax2 ay2) = getBox2 symbObj1
        (V2 bx1 by1, V2 bx2 by2) = getBox2 symbObj2
    in
        (V3 (bx1+ax1) (by1+ax1) ay1, V3 (bx2+ax2) (by2+ax2) ay2)
-- FIXME: magic numbers: 0.2 and 11.
-- FIXME: this may use an approximation, based on sampling functions. generate a warning if the approximation part of this function is used.
-- FIXME: re-implement the expression system, so this can recieve a function, and determine class (constant, linear)... and implement better forms of this function.
getBox3 (ExtrudeM twist scale translate symbObj height) =
    let
        (V2 x1 y1, V2 x2 y2) = getBox2 symbObj
        (dx, dy) = (x2 - x1, y2 - y1)

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
                    hs = [hfun $ V2 x y | x <- xrange, y <- yrange]
                    (hmin, hmax) = (minimum hs, maximum hs)
        hrange = fmap ((/ fromFastℕtoℝ (samples-1)) . (h*) . fromFastℕtoℝ) range

        (twistXmin, twistYmin, twistXmax, twistYmax) =
          let
            both f (a, b) = (f a, f b)
            (V2 scalex' scaley') = case scale of
              C1 s -> V2 s s
              C2 s -> s
              s -> pack $ both maximum . unzip $ fmap unpack $ fmap abs . toScaleFn s <$> hrange
            smin s v = min v (s * v)
            smax s v = max v (s * v)
            -- FIXME: assumes minimums are negative, and maximums are positive.
            scaleEach (V2 d1 d2, V2 d3 d4) = (scalex' * d1, scaley' * d2, scalex' * d3, scaley' * d4)
          in case twist of
            Left twval -> if twval == 0
                          then (smin scalex' x1, smin scaley' y1, smax scalex' x2, smax scaley' y2)
                          else scaleEach $ getBox2R symbObj twval
            Right _  -> scaleEach $ getBox2R symbObj 360 -- we can't range functions yet, so assume a full circle.

        (tminx, tmaxx, tminy, tmaxy) =
          let
            tvalsx :: (ℝ -> V2 ℝ) -> [ℝ]
            tvalsx tfun = fmap (fst . unpack . tfun) hrange
            tvalsy :: (ℝ -> V2 ℝ) -> [ℝ]
            tvalsy tfun = fmap (snd . unpack . tfun) hrange
          in case translate of
            Left  (V2 tvalx tvaly) -> (tvalx, tvalx, tvaly, tvaly)
            Right tfun -> ( minimum $ tvalsx tfun
                          , maximum $ tvalsx tfun
                          , minimum $ tvalsy tfun
                          , maximum $ tvalsy tfun
                          )
    in
        (V3 (twistXmin + tminx) (twistYmin + tminy) 0, V3 (twistXmax + tmaxx) (twistYmax + tmaxy) h)
-- Note: Assumes x2 is always greater than x1.
-- FIXME: Insert the above assumption as an assertion in the type system?
getBox3 (RotateExtrude _ _ (Left (V2 xshift yshift)) _ symbObj) =
    let
        (V2 _ y1, V2 x2 y2) = getBox2 symbObj
        r = max x2 (x2 + xshift)
    in
        (V3 (-r) (-r) $ min y1 (y1 + yshift), V3 r r $ max y2 (y2 + yshift))
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
        (V2 x1 y1, V2 x2 y2) = getBox2 symbObj
        (xrange, yrange) = unzip $ fmap unpack $ take (fromFastℕ samples) $ fmap (f . (step*) . fromFastℕtoℝ) range
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
        (V3 (-r) (-r) $ y1 + ymin', V3 r  r  $ y2 + ymax')
-- FIXME: add case for ExtrudeRotateR!
getBox3 ExtrudeRotateR{} = error "ExtrudeRotateR implementation incomplete!"


unpack :: V2 a -> (a, a)
unpack (V2 a b) = (a, b)

pack :: (a, a) -> V2 a
pack = uncurry V2

