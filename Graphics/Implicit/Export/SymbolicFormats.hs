{-# LANGUAGE OverloadedStrings #-}

-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

module Graphics.Implicit.Export.SymbolicFormats where

import Graphics.Implicit.Definitions
import Graphics.Implicit.Export.TextBuilderUtils

import Control.Monad.Reader

import Data.List (intersperse)


scad2 :: ℝ -> SymbolicObj2 -> Text 
scad2 res obj = toLazyText $ runReader (buildS2 obj) res

scad3 :: ℝ -> SymbolicObj3 -> Text 
scad3 res obj = toLazyText $ runReader (buildS3 obj) res



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

call      = callToken ("[", "]")
callNaked = callToken ("",   "")


buildS3 :: SymbolicObj3 -> Reader ℝ Builder

buildS3 (Rotate3 (x,y,z) obj) = call "rotate" [bf (rad2deg x), bf (rad2deg y), bf (rad2deg z)] [buildS3 obj]
  where
    rad2deg r = r * (180/pi)

buildS3 (UnionR3 0 objs) = call "union" [] $ map buildS3 objs

buildS3 (DifferenceR3 0 objs) = call "difference" [] $ map buildS3 objs

buildS3 (IntersectR3 0 objs)  = call " intersection" [] $ map buildS3 objs

buildS3 (Translate3 (x,y,z) obj) = call "translate" [bf x, bf y, bf z] [buildS3 obj]

buildS3 (Scale3 (x,y,z) obj) = call "scale" [bf x, bf y, bf z] [buildS3 obj]

buildS3 (Rect3R 0 (x1,y1,z1) (x2,y2,z2)) = call "translate" [bf x1, bf y1, bf z1] [
                                            call "cube" [bf $ x2 - x1, bf $ y2 - y1, bf $ z2 - z1] []
                                           ]
buildS3 (Cylinder h r1 r2) = call "cylinder" [
                              "r1 = " <> bf r1
                             ,"r2 = " <> bf r2
                             , bf h
                             ] []

buildS3 (Sphere r) = callNaked "sphere" ["r = " <> bf r] []

buildS3 (ExtrudeR 0 obj h) = callNaked "linear_extrude" ["height = " <> bf h] [buildS2 obj]

buildS3 (ExtrudeRotateR 0 twist obj h) =
    callNaked "linear_extrude" ["height = " <> bf h, "twist = " <> bf twist] [buildS2 obj]

buildS3 (ExtrudeRM 0 (Just twist) Nothing Nothing obj (Left height)) = do
  res <- ask
  call "union" [] [
             call "rotate" ["0","0", bf $ twist h] [
                        callNaked "linear_extrude" ["height = " <> bf res, "twist = " <> bf (twist (h+res) - twist h)][
                                   buildS2 obj
                                  ]                         
                       ] |  h <- init [0, res .. height]
            ]

buildS2 :: SymbolicObj2 -> Reader ℝ Builder

buildS2 (UnionR2 0 objs)       = call "union" [] $ map buildS2 objs

buildS2 (DifferenceR2 0 objs)  = call "difference" [] $ map buildS2 objs

buildS2 (IntersectR2 0 objs)   = call "intersection" [] $ map buildS2 objs

buildS2 (Translate2 (x,y) obj) = call "translate" [bf x, bf y] $ [buildS2 obj]

buildS2 (Scale2 (x,y) obj)     = call "scale" [bf x, bf y] $ [buildS2 obj]

buildS2 (RectR 0 (x1,y1) (x2,y2)) = call "translate" [bf x1, bf y1] [
                                    call "cube" [bf $ x2 - x1, bf $ y2 - y1] []
                                   ]

buildS2 (Circle r) = call "circle" [bf r] []

buildS2 (PolygonR 0 points) = call "polygon" [buildVector [x,y] | (x,y) <- points] []
    where buildVector comps = "[" <> mconcat (intersperse "," $ map bf comps) <> "]"


