-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright (C) 2016 Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

-- FIXME: describe why we need this.
{-# LANGUAGE OverloadedStrings #-}

-- output SCAD code, AKA an implicitcad to openscad converter.
module Graphics.Implicit.Export.SymbolicFormats (scad2, scad3) where

import Prelude(Maybe(Just, Nothing), Either(Left), ($), (.), (*), map, ($!), (-), (/), pi, error, (+), init, (==))

import Graphics.Implicit.Definitions(ℝ, SymbolicObj2(RectR, Circle, PolygonR, Complement2, UnionR2, DifferenceR2, IntersectR2, Translate2, Scale2, Rotate2, Outset2, Shell2, EmbedBoxedObj2), SymbolicObj3(Rect3R, Sphere, Cylinder, Complement3, UnionR3, IntersectR3, DifferenceR3, Translate3, Scale3, Rotate3, Rotate3V, Outset3, Shell3, ExtrudeR, ExtrudeRotateR, ExtrudeRM, EmbedBoxedObj3, RotateExtrude, ExtrudeOnEdgeOf))
import Graphics.Implicit.Export.TextBuilderUtils(Text, Builder, toLazyText, (<>), mconcat, fromLazyText, bf)

import Control.Monad.Reader (Reader, runReader, return, fmap, sequence, ask)

import Data.List (intersperse)

scad2 :: ℝ -> SymbolicObj2 -> Text 
scad2 res obj = toLazyText $ runReader (buildS2 obj) res

scad3 :: ℝ -> SymbolicObj3 -> Text 
scad3 res obj = toLazyText $ runReader (buildS3 obj) res

-- used by rotate2 and rotate3
rad2deg :: ℝ -> ℝ
rad2deg r = r * (180/pi)

-- Format an openscad call given that all the modified objects are in the Reader monad...

callToken :: (Text, Text) -> Builder -> [Builder] -> [Reader a Builder] -> Reader a Builder
callToken cs name args []    = return $ name <> buildArgs cs args <> ";"
callToken cs name args [obj] = fmap ((name <> buildArgs cs args) <>) obj
callToken cs name args objs  = do
  objs' <- fmap (mconcat . map (<> "\n")) $ sequence objs
  return $! name <> buildArgs cs args <> "{\n" <> objs' <> "}\n"

buildArgs :: (Text, Text) -> [Builder] -> Builder
buildArgs _ [] = "()"
buildArgs (c1, c2) args = "(" <> (fromLazyText c1) <> mconcat (intersperse "," args) <> (fromLazyText c2) <> ")"

call :: Builder -> [Builder] -> [Reader a Builder] -> Reader a Builder
call = callToken ("[", "]")

callNaked :: Builder -> [Builder] -> [Reader a Builder] -> Reader a Builder
callNaked = callToken ("", "")

-- First, the 3D objects.
buildS3 :: SymbolicObj3 -> Reader ℝ Builder

buildS3 (Rect3R r (x1,y1,z1) (x2,y2,z2)) | r == 0 = call "translate" [bf x1, bf y1, bf z1] [
                                            call "cube" [bf $ x2 - x1, bf $ y2 - y1, bf $ z2 - z1] []
                                           ]

buildS3 (Sphere r) = callNaked "sphere" ["r = " <> bf r] []

buildS3 (Cylinder h r1 r2) = callNaked "cylinder" [
                              "r1 = " <> bf r1
                             ,"r2 = " <> bf r2
                             , bf h
                             ] []

buildS3 (Complement3 obj) = call "complement" [] [buildS3 obj]

buildS3 (UnionR3 r objs) | r == 0 = call "union" [] $ map buildS3 objs

buildS3 (IntersectR3 r objs) | r == 0 = call "intersection" [] $ map buildS3 objs

buildS3 (DifferenceR3 r objs) | r == 0 = call "difference" [] $ map buildS3 objs

buildS3 (Translate3 (x,y,z) obj) = call "translate" [bf x, bf y, bf z] [buildS3 obj]

buildS3 (Scale3 (x,y,z) obj) = call "scale" [bf x, bf y, bf z] [buildS3 obj]

buildS3 (Rotate3 (x,y,z) obj) = call "rotate" [bf (rad2deg x), bf (rad2deg y), bf (rad2deg z)] [buildS3 obj]

-- FIXME: where is Rotate3V?
buildS3 (Rotate3V _ _ _) = error "Rotate3V not implemented."

buildS3 (Outset3 r obj) | r == 0 = call "outset" [] [buildS3 obj]

buildS3 (Shell3 r obj) | r == 0 = call "shell" [] [buildS3 obj]

-- FIXME: where is EmbedBoxedObj3?

buildS3 (ExtrudeR r obj h) | r == 0 = callNaked "linear_extrude" ["height = " <> bf h] [buildS2 obj]

buildS3 (ExtrudeRotateR r twist obj h) | r == 0 = callNaked "linear_extrude" ["height = " <> bf h, "twist = " <> bf twist] [buildS2 obj]

buildS3 (ExtrudeRM r (Just twist) Nothing Nothing obj (Left height)) | r == 0 = do
  res <- ask
  call "union" [] [
             call "rotate" ["0","0", bf $ twist h] [
                        callNaked "linear_extrude" ["height = " <> bf res, "twist = " <> bf (twist (h+res) - twist h)][
                                   buildS2 obj
                                  ]                         
                       ] |  h <- init [0, res .. height]
            ]

-- FIXME: where are RotateExtrude, ExtrudeOnEdgeOf?

buildS3(Rect3R _ _ _) = error "cannot provide roundness when exporting openscad; unsupported in target format."
buildS3(UnionR3 _ _) = error "cannot provide roundness when exporting openscad; unsupported in target format."
buildS3(IntersectR3 _ _) = error "cannot provide roundness when exporting openscad; unsupported in target format."
buildS3(DifferenceR3 _ _) = error "cannot provide roundness when exporting openscad; unsupported in target format."
buildS3(Outset3 _ _) = error "cannot provide roundness when exporting openscad; unsupported in target format."
buildS3(Shell3 _ _) = error "cannot provide roundness when exporting openscad; unsupported in target format."
buildS3(ExtrudeR _ _ _) = error "cannot provide roundness when exporting openscad; unsupported in target format."
buildS3(ExtrudeRotateR _ _ _ _) = error "cannot provide roundness when exporting openscad; unsupported in target format."
buildS3(ExtrudeRM _ _ _ _ _ _) = error "cannot provide roundness when exporting openscad; unsupported in target format."
buildS3(EmbedBoxedObj3 _) = error "cannot provide roundness when exporting openscad; unsupported in target format."
buildS3(RotateExtrude _ _ _ _ _) = error "cannot provide roundness when exporting openscad; unsupported in target format."
buildS3(ExtrudeOnEdgeOf _ _) = error "cannot provide roundness when exporting openscad; unsupported in target format."

-- Now the 2D objects/transforms.

buildS2 :: SymbolicObj2 -> Reader ℝ Builder

buildS2 (RectR r (x1,y1) (x2,y2)) | r == 0 = call "translate" [bf x1, bf y1] [
                                    call "cube" [bf $ x2 - x1, bf $ y2 - y1] []
                                   ]

buildS2 (Circle r) = call "circle" [bf r] []

buildS2 (PolygonR r points) | r == 0 = call "polygon" [buildVector [x,y] | (x,y) <- points] []
    where buildVector comps = "[" <> mconcat (intersperse "," $ map bf comps) <> "]"

buildS2 (Complement2 obj) = call "complement" [] $ [buildS2 obj]

buildS2 (UnionR2 r objs) | r == 0 = call "union" [] $ map buildS2 objs

buildS2 (DifferenceR2 r objs) | r == 0 = call "difference" [] $ map buildS2 objs

buildS2 (IntersectR2 r objs) | r == 0 = call "intersection" [] $ map buildS2 objs

buildS2 (Translate2 (x,y) obj) = call "translate" [bf x, bf y] $ [buildS2 obj]

buildS2 (Scale2 (x,y) obj)     = call "scale" [bf x, bf y] $ [buildS2 obj]

buildS2 (Rotate2 (r) obj)     = call "rotate" [bf (rad2deg r)] $ [buildS2 obj]

buildS2 (Outset2 r obj) | r == 0 = call "outset" [] $ [buildS2 obj]

buildS2 (Shell2 r obj) | r == 0 =  call "shell" [] $ [buildS2 obj]

-- Generate errors for rounding requests. OpenSCAD does not support rounding.
buildS2 (RectR _ _ _) = error "cannot provide roundness when exporting openscad; unsupported in target format."
buildS2 (PolygonR _ _) = error "cannot provide roundness when exporting openscad; unsupported in target format."
buildS2 (UnionR2 _ _) = error "cannot provide roundness when exporting openscad; unsupported in target format."
buildS2 (DifferenceR2 _ _) = error "cannot provide roundness when exporting openscad; unsupported in target format."
buildS2 (IntersectR2 _ _) = error "cannot provide roundness when exporting openscad; unsupported in target format."
buildS2 (Outset2 _ _) = error "cannot provide roundness when exporting openscad; unsupported in target format."
buildS2 (Shell2 _ _) = error "cannot provide roundness when exporting openscad; unsupported in target format."

-- FIXME: missing EmbedBoxedObj2?
buildS2 (EmbedBoxedObj2 _) = error "EmbedBoxedObj2 not implemented."

