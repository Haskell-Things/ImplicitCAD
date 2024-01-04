{- ORMOLU_DISABLE -}
-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright (C) 2016, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

{-# LANGUAGE OverloadedStrings #-}

module Graphics.Implicit.Export.PolylineFormats (svg, hacklabLaserGCode, dxf2) where

import Prelude((.), ($), (-), (+), (/), minimum, maximum, unzip, show, unwords, fmap, snd, compare, min, max, length, foldl, mempty, (<>), (<$>))

import Graphics.Implicit.Definitions (Polyline(Polyline), ℝ, ℝ2)

import Graphics.Implicit.Export.TextBuilderUtils (Text, Builder, toLazyText, bf, buildInt, buildTruncFloat, fromLazyText)

import Text.Blaze.Svg.Renderer.Text (renderSvg)
import Text.Blaze.Svg11 ((!),docTypeSvg,g,polyline,toValue,Svg)
import Text.Blaze.Internal (stringValue)
import qualified Text.Blaze.Svg11.Attributes as A (version, width, height, viewbox, points, stroke, strokeWidth, fill)

import Data.List (sortBy, foldl')

import Data.Foldable (fold, foldMap, traverse_)
import Linear ( V2(V2) )

default (ℝ)

-- FIXME: magic numbers.
svg :: [Polyline] -> Text
svg plines = renderSvg . svg11 . svg' $ plines
    where
      strokeWidth :: ℝ
      strokeWidth = 1
      (xmin, xmax, ymin, ymax) = (xmin' - margin, xmax' + margin, ymin' - margin, ymax' + margin)
           where margin = strokeWidth / 2
                 ((xmin', xmax'), (ymin', ymax')) = (maxMinList xs,maxMinList ys)
                 xs, ys :: [ℝ]
                 (xs,ys) = unzip $ unpack <$> foldMap pair plines
                 pair (Polyline a) = a
                 maxMinList :: [ℝ] -> (ℝ,ℝ)
                 maxMinList (x:others) = foldl (\(l,h) y -> (min l y, max h y)) (x,x) others
                 maxMinList [] = (0,0)
      svg11 = docTypeSvg ! A.version "1.1"
                         ! A.width  (stringValue $ show (xmax-xmin) <> "mm")
                         ! A.height (stringValue $ show (ymax-ymin) <> "mm")
                         ! A.viewbox (stringValue $ unwords $ show <$> [0,0,xmax-xmin,ymax-ymin])

      -- The reason this isn't totally straightforwards is that svg has different coordinate system
      -- and we need to compute the requisite translation.
      svg' :: [Polyline] -> Svg
      svg' [] = mempty
      -- When we have a known point, we can compute said transformation:
      svg' polylines = thinBlueGroup $ traverse_ poly polylines

      poly (Polyline line) = polyline ! A.points pointList
          where pointList = toValue $ toLazyText $ fold [fromLazyText (bf $ x-xmin) <> "," <> fromLazyText (bf $ ymax - y) <> " " | (V2 x y) <- line]

      -- Instead of setting styles on every polyline, we wrap the lines in a group element and set the styles on it:
      thinBlueGroup = g ! A.stroke "rgb(0,0,255)" ! A.strokeWidth (stringValue $ show strokeWidth) ! A.fill "none" -- obj

-- | DXF2 export in 2D. conforming to AutoCAD R12/13.
dxf2 :: [Polyline] -> Text
dxf2 plines = toLazyText $ dxf2Header <> dxf2Tables <> dxf2Blocks <> dxf2Entities
     where
      dxf2Header :: Builder
      dxf2Header =
        "  0\n" <> "SECTION\n" <>
        "  2\n" <> "HEADER\n" <>
        "  9\n" <> "$ACADVER\n" <>
        "  1\n" <> "AC1009\n" <>
        "  9\n" <> "$LIMMIN\n" <>
        " 10\n" <> buildTruncFloat dxfxmin <> "\n" <>
        " 20\n" <> buildTruncFloat dxfymin <> "\n" <>
        "  9\n" <> "$LIMMAX\n" <>
        " 10\n" <> buildTruncFloat dxfxmax <> "\n" <>
        " 20\n" <> buildTruncFloat dxfymax <> "\n" <>
        "  9\n" <> "$LUPREC\n" <>
        " 70\n" <> "4\n" <>
        "  0\n" <> "ENDSEC\n"
      dxf2Tables :: Builder
      dxf2Tables =
        "  0\n" <> "SECTION\n" <>
        "  2\n" <> "TABLES\n" <>
        "  0\n" <> "ENDSEC\n"
      dxf2Blocks :: Builder
      dxf2Blocks =
        "  0\n" <> "SECTION\n" <>
        "  2\n" <> "BLOCKS\n" <>
        "  0\n" <> "ENDSEC\n"
      dxf2Entities :: Builder
      dxf2Entities =
        "  0\n" <> "SECTION\n" <>
        "  2\n" <> "ENTITIES\n" <>
        foldMap buildPolyline (orderPolylines plines) <>
        "  0\n" <> "ENDSEC\n"
      buildPolyline :: Polyline -> Builder
      buildPolyline (Polyline singlePolyline) =
        "  0\n" <> "POLYLINE\n" <>
        "  8\n" <> "0\n" <>
        "  6\n" <> "CONTINUOUS\n" <>
        " 66\n" <> "1\n" <>
        " 62\n" <> buildInt (length singlePolyline) <> "\n" <>
        " 10\n" <> "0.0\n" <>
        " 20\n" <> "0.0\n" <>
        " 30\n" <> "0.0000\n" <>
        foldMap buildVertex singlePolyline <>
        "  0\n" <> "SEQEND\n"
      buildVertex :: ℝ2 -> Builder
      buildVertex (V2 x1 y1) =
        "  0\n" <>"VERTEX\n" <>
        "  8\n" <>"0\n" <>
        "  10\n" <> buildTruncFloat x1 <> "\n" <>
        "  20\n" <> buildTruncFloat y1 <> "\n"
      (dxfxmin, dxfxmax, dxfymin, dxfymax) = (minimum xs, maximum xs, minimum ys, maximum ys)
      (xs, ys) = unzip $ unpack <$> foldMap pair plines
      pair :: Polyline -> [ℝ2]
      pair (Polyline x) = x

orderPolylines :: [Polyline] -> [Polyline]
orderPolylines =
  fmap snd . sortBy (\(a,_) (b, _) -> compare a b) . fmap (\x -> (polylineRadius x, x))
  where
    polylineRadius :: Polyline -> ℝ
    polylineRadius polyline' = max (xmax' - xmin') (ymax' - ymin')
      where
        (V2 xmin'  xmax', V2 ymin' ymax') = polylineRadius' [polyline']
        polylineRadius' :: [Polyline] -> (ℝ2, ℝ2)
        polylineRadius' lines = (maxMinList xs,maxMinList ys)
          where
            (xs,ys) = unzip $ unpack <$> foldMap pair lines
            pair (Polyline a) = a
            maxMinList :: [ℝ] -> ℝ2
            maxMinList (x:others) = foldl' (\(V2 l h) y -> V2 (min l y) (max h y)) (V2 x x) others
            maxMinList [] = V2 0 0

unpack :: V2 a -> (a, a)
unpack (V2 x y) = (x, y)

-- | Gcode generation for the laser cutter in HackLab. Complies with https://ws680.nist.gov/publication/get_pdf.cfm?pub_id=823374
--   FIXME: parameters would be nice.
hacklabLaserGCode :: [Polyline] -> Text
hacklabLaserGCode polylines = toLazyText $ gcodeHeader <> foldMap interpretPolyline (orderPolylines polylines) <> gcodeFooter
    where
      gcodeHeader :: Builder
      gcodeHeader = "(generated by ImplicitCAD, based of hacklab wiki example)\n"
                    <> "M63 P0 (laser off)\n"
                    <> "G0 Z0.002 (laser off)\n"
                    <> "G21 (units=mm)\n"
                    <> "F400 (set feedrate)\n"
                    <> "M3 S1 (enable laser)\n\n"
      gcodeFooter :: Builder
      gcodeFooter = "M5 (disable laser)\n"
                    <> "G00 X0.0 Y0.0 (move to 0)\n"
                    <> "M2 (end)"
      gcodeXY :: ℝ2 -> Builder
      gcodeXY (V2 x y) = "X" <> buildTruncFloat x <> " Y" <> buildTruncFloat y
      interpretPolyline :: Polyline -> Builder
      interpretPolyline (Polyline (start:others)) =
        "G00 " <> gcodeXY start
        <> "\nM62 P0 (laser on)\n"
        <> fold [ "G01 " <> gcodeXY point <> "\n" | point <- others]
        <> "M63 P0 (laser off)\n\n"
      interpretPolyline (Polyline []) = mempty
