-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright (C) 2016 Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

-- FIXME: describe why we need this.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- output SCAD code, AKA an implicitcad to openscad converter.
module Graphics.Implicit.Export.SymbolicFormats (scad2, scad3) where

import Prelude((.), fmap, Either(Left, Right), ($), (*), (-), (/), pi, error, (+), (==), take, floor, (&&), const, (<>), (<$>))

import Graphics.Implicit.Definitions(ℝ, SymbolicObj2(Shared2, Square, Circle, Polygon, Rotate2, Transform2, Slice), SymbolicObj3(Shared3, Cube, Sphere, Cylinder, BoxFrame, Rotate3, Transform3, Extrude, ExtrudeM, RotateExtrude, ExtrudeOnEdgeOf, Torus, Ellipsoid, Link), isScaleID, SharedObj(Empty, Full, Complement, UnionR, IntersectR, DifferenceR, Translate, Scale, Mirror, Outset, Shell, EmbedBoxedObj, WithRounding), quaternionToEuler)
import Graphics.Implicit.Export.TextBuilderUtils(Text, bf)

-- For constructing vectors of ℝs.
import Linear (V2(V2), V3(V3), V4(V4))

import Data.List (intersperse)
import Data.Function (fix)
import Data.Foldable(fold, toList)
import Graphics.Implicit.ObjectUtil.GetBoxShared (VectorStuff(elements))
import Prettyprinter (Doc, (<+>), vsep, layoutPretty, defaultLayoutOptions, Pretty (pretty), concatWith, nest, line)
import Prettyprinter.Render.Text (renderLazy)

default (ℝ)

scad2 :: ℝ -> SymbolicObj2 -> Text
scad2 res obj = renderLazy . layoutPretty defaultLayoutOptions $ buildS2 res obj

scad3 :: ℝ -> SymbolicObj3 -> Text
scad3 res obj = renderLazy . layoutPretty defaultLayoutOptions $ buildS3 res obj

-- used by rotate2 and rotate3
rad2deg :: ℝ -> ℝ
rad2deg r = r * (180/pi)

-- | Format an openscad call given that all the modified objects are in the Reader monad...
callToken :: (Doc a, Doc a) -> Doc a -> [Doc a] -> [Doc a] -> Doc a
callToken cs name args []    = name <> buildArgs cs args <> ";"
callToken cs name args [obj] = name <> buildArgs cs args <+> obj
callToken cs name args objs  = vsep
  -- nest doesn't indent the first element in the Doc, so we can use the calling name
  -- as our first line, and add an extra line break in to match the `vsep` layout.
  [ nest 4 $
    (name <> buildArgs cs args <+> "{") <> line
      <> vsep objs
  , "}"
  ]

buildArgs :: (Doc a, Doc a) -> [Doc a] -> Doc a
buildArgs _ [] = "()"
buildArgs (c1, c2) args = "(" <> c1 <> concatWith (\a b -> a <> "," <+> b) args <> c2 <> ")"

call :: Doc a -> [Doc a] -> [Doc a] -> Doc a
call = callToken ("[", "]")

callNaked :: Doc a -> [Doc a] -> [Doc a] -> Doc a
callNaked = callToken ("", "")

------------------------------------------------------------------------------
-- | Class which allows us to build the contained objects in 'buildShared'.
class Build obj where
  build :: ℝ -> obj -> Doc ()

instance Build SymbolicObj2 where
  build = buildS2

instance Build SymbolicObj3 where
  build = buildS3

------------------------------------------------------------------------------
-- | Unpack a dimensionality-polymorphic vector into multiple arguments.
vectAsArgs :: VectorStuff vec => vec -> [Doc a]
vectAsArgs = fmap (pretty . bf) . elements

------------------------------------------------------------------------------
-- | Unpack a dimensionality-polymorphic vector into a single argument.
bvect :: VectorStuff vec => vec -> Doc a
bvect v = "[" <> fold (intersperse "," $ vectAsArgs v) <> "]"

------------------------------------------------------------------------------
-- | Build the common combinators.
buildShared :: forall obj f a. (Build obj, VectorStuff (f a)) => ℝ -> SharedObj obj f a -> Doc ()

buildShared _ Empty = call "union" [] []

buildShared _ Full = call "difference" [] [call "union" [] []]

buildShared res (Complement obj) = call "complement" [] [build res obj]

buildShared res (UnionR r objs) | r == 0 = call "union" [] $ build res <$> objs

buildShared res (IntersectR r objs) | r == 0 = call "intersection" [] $ build res <$> objs

buildShared res (DifferenceR r obj objs) | r == 0 = call "difference" [] $ build res <$> obj : objs

buildShared res (Translate v obj) = call "translate" (pretty . bf <$> elements v) [build res obj]

buildShared res (Scale v obj) = call "scale" (pretty . bf <$> elements v) [build res obj]

buildShared res (Mirror v obj) = callNaked "mirror" [ "v=" <> bvect v ] [build res obj]

-- NOTE(sandy): This @r == 0@ guard says we only emit "outset" if it has r = 0,
-- erroring otherwise saying "cannot provide roundness." But this is not
-- a roundness parameter!
buildShared res (Outset r obj) | r == 0 = call "outset" [] [build res obj]

-- NOTE(sandy): This @r == 0@ guard says we only emit "shell" if it has r = 0,
-- erroring otherwise saying "cannot provide roundness." But this is not
-- a roundness parameter!
buildShared res (Shell r obj) | r == 0 = call "shell" [] [build res obj]

buildShared res (WithRounding r obj) | r == 0 = build res obj

buildShared _ (UnionR _ _) = error "cannot provide roundness when exporting openscad; unsupported in target format."
buildShared _ (IntersectR _ _) = error "cannot provide roundness when exporting openscad; unsupported in target format."
buildShared _ (DifferenceR {}) = error "cannot provide roundness when exporting openscad; unsupported in target format."
buildShared _ (Outset _ _) = error "cannot provide roundness when exporting openscad; unsupported in target format."
buildShared _ (Shell _ _) = error "cannot provide roundness when exporting openscad; unsupported in target format."
buildShared _ (EmbedBoxedObj _) = error "cannot provide roundness when exporting openscad; unsupported in target format."
buildShared _ (WithRounding _ _) = error "cannot provide roundness when exporting openscad; unsupported in target format."

-- | First, the 3D objects.
buildS3 :: ℝ -> SymbolicObj3 -> Doc ()

buildS3 res (Shared3 obj) = buildShared res obj

buildS3 _ (Cube (V3 w d h)) = call "cube" [pretty $ bf w, pretty $ bf d, pretty $ bf h] []

buildS3 _ (Sphere r) = callNaked "sphere" ["r = " <> pretty (bf r)] []

buildS3 _ (Cylinder h r1 r2) = callNaked "cylinder" [
                              "r1 = " <> pretty (bf r1)
                             ,"r2 = " <> pretty (bf r2)
                             , pretty $ bf h
                             ] []
buildS3 res (Rotate3 q obj) =
  let (V3 x y z) = quaternionToEuler q
   in call "rotate" [pretty $ bf (rad2deg x), pretty $ bf (rad2deg y), pretty $ bf (rad2deg z)] [buildS3 res obj]

buildS3 res (Transform3 m obj) =
    call "multmatrix"
      ((\x -> "["<>x<>"]") . fold . intersperse "," . fmap (pretty . bf) . toList <$> toList m)
      [buildS3 res obj]

buildS3 res (Extrude h obj) = callNaked "linear_extrude" ["height = " <> pretty (bf h)] [buildS2 res obj]

-- FIXME: handle scale, center.
buildS3 res (ExtrudeM twist scale (Left translate) obj (Left height)) |isScaleID scale && translate == V2 0 0 =
  let
    twist' = case twist of
               Left twval  -> const twval
               Right twfun -> twfun
  in call "union" [] [
             call "rotate" ["0","0", pretty $ bf $ twist' h] [
                        callNaked "linear_extrude" ["height = " <> pretty (bf res), "twist = " <> pretty (bf $ twist' (h+res) - twist' h)] [
                                   buildS2 res obj
                                  ]
                       ] |  h <- take (floor (res / height)) $ fix (\f x -> x : f (x+res)) 0
            ]

-- FIXME: where are RotateExtrude, ExtrudeOnEdgeOf?

buildS3 _ ExtrudeM{} = error "cannot provide roundness when exporting openscad; unsupported in target format."
buildS3 _ RotateExtrude{} = error "cannot provide roundness when exporting openscad; unsupported in target format."
buildS3 _ (ExtrudeOnEdgeOf _ _) = error "cannot provide roundness when exporting openscad; unsupported in target format."
-- Extended primitives that aren't supported in openscad
buildS3 _ Torus{} = error "cannot use torus objects when exporting openscad; unsupported in target format."
buildS3 _ Ellipsoid{} = error "cannot use ellipsoid objects when exporting openscad; unsupported in target format."
buildS3 _ BoxFrame{} = error "cannot use boxFrame objects when exporting openscad; unsupported in target format."
buildS3 _ Link{} = error "cannot use link objects when exporting openscad; unsupported in target format."

-- Now the 2D objects/transforms.

buildS2 :: ℝ -> SymbolicObj2 -> Doc ()

buildS2 res (Shared2 obj) = buildShared res obj

buildS2 _ (Circle r) = call "circle" [pretty $ bf r] []

buildS2 _ (Polygon points) = call "polygon" (fmap bvect points) []

buildS2 res (Rotate2 r obj)     = call "rotate" [pretty $ bf (rad2deg r)] [buildS2 res obj]

buildS2 res (Transform2 m obj) =
    let toM44 (V3 (V3 a b c) (V3 d e f) (V3 g h i)) =
          V4 (V4 a b c 0)
             (V4 d e f 0)
             (V4 g h i 0)
             (V4 0 0 0 1)
    in
    call "multmatrix"
      ((\x -> "["<>x<>"]") . fold . intersperse "," . fmap (pretty . bf) . toList <$> toList (toM44 m))
      [buildS2 res obj]

buildS2 _ (Square (V2 w h)) = call "square" [pretty $ bf w, pretty $ bf h] []

buildS2 res (Slice obj) = callNaked "projection" ["cut = true"] [buildS3 res obj]
