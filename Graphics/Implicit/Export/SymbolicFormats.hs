-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright (C) 2016 Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

-- FIXME: describe why we need this.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- output SCAD code, AKA an implicitcad to openscad converter.
module Graphics.Implicit.Export.SymbolicFormats (scad2, scad3) where

import Prelude((.), fmap, Either(Left, Right), ($), (*), ($!), (-), (/), pi, error, (+), (==), take, floor, (&&), const, pure, (<>), sequenceA, (<$>))

import Graphics.Implicit.Definitions(ℝ, SymbolicObj2(Shared2, Square, Circle, Polygon, Rotate2), SymbolicObj3(Shared3, Cube, Sphere, Cylinder, Rotate3, Extrude, ExtrudeRotateR, ExtrudeM, RotateExtrude, ExtrudeOnEdgeOf), isScaleID, SharedObj(Empty, Full, Complement, UnionR, IntersectR, DifferenceR, Translate, Scale, Mirror, Outset, Shell, EmbedBoxedObj, WithRounding), quaternionToEuler)
import Graphics.Implicit.Export.TextBuilderUtils(Text, Builder, toLazyText, fromLazyText, bf)

import Control.Monad.Reader (Reader, runReader, ask)

-- For constructing vectors of ℝs.
import Linear (V3(V3), V2(V2))

import Data.List (intersperse)
import Data.Function (fix)
import Data.Foldable(fold, foldMap)
import Graphics.Implicit.ObjectUtil.GetBoxShared (VectorStuff(elements))

default (ℝ)

scad2 :: ℝ -> SymbolicObj2 -> Text
scad2 res obj = toLazyText $ runReader (buildS2 obj) res

scad3 :: ℝ -> SymbolicObj3 -> Text
scad3 res obj = toLazyText $ runReader (buildS3 obj) res

-- used by rotate2 and rotate3
rad2deg :: ℝ -> ℝ
rad2deg r = r * (180/pi)

-- | Format an openscad call given that all the modified objects are in the Reader monad...
callToken :: (Text, Text) -> Builder -> [Builder] -> [Reader a Builder] -> Reader a Builder
callToken cs name args []    = pure $ name <> buildArgs cs args <> ";"
callToken cs name args [obj] = ((name <> buildArgs cs args) <>) <$> obj
callToken cs name args objs  = do
  objs' <- foldMap (<> "\n") <$> sequenceA objs
  pure $! name <> buildArgs cs args <> "{\n" <> objs' <> "}\n"

buildArgs :: (Text, Text) -> [Builder] -> Builder
buildArgs _ [] = "()"
buildArgs (c1, c2) args = "(" <> fromLazyText c1 <> fold (intersperse "," args) <> fromLazyText c2 <> ")"

call :: Builder -> [Builder] -> [Reader a Builder] -> Reader a Builder
call = callToken ("[", "]")

callNaked :: Builder -> [Builder] -> [Reader a Builder] -> Reader a Builder
callNaked = callToken ("", "")


------------------------------------------------------------------------------
-- | Class which allows us to build the contained objects in 'buildShared'.
class Build obj where
  build :: obj -> Reader ℝ Builder

instance Build SymbolicObj2 where
  build = buildS2

instance Build SymbolicObj3 where
  build = buildS3


------------------------------------------------------------------------------
-- | Unpack a dimensionality-polymorphic vector into multiple arguments.
vectAsArgs :: VectorStuff vec => vec -> [Builder]
vectAsArgs = fmap bf . elements

------------------------------------------------------------------------------
-- | Unpack a dimensionality-polymorphic vector into a single argument.
bvect :: VectorStuff vec => vec -> Builder
bvect v = "[" <> fold (intersperse "," $ vectAsArgs v) <> "]"


------------------------------------------------------------------------------
-- | Build the common combinators.
buildShared :: forall obj vec. (Build obj, VectorStuff vec) => SharedObj obj vec -> Reader ℝ Builder

buildShared Empty = call "union" [] []

buildShared Full = call "difference" [] [call "union" [] []]

buildShared (Complement obj) = call "complement" [] [build obj]

buildShared (UnionR r objs) | r == 0 = call "union" [] $ build <$> objs

buildShared (IntersectR r objs) | r == 0 = call "intersection" [] $ build <$> objs

buildShared (DifferenceR r obj objs) | r == 0 = call "difference" [] $ build <$> obj : objs

buildShared (Translate v obj) = call "translate" (bf <$> elements v) [build obj]

buildShared (Scale v obj) = call "scale" (bf <$> elements v) [build obj]

buildShared (Mirror v obj) = callNaked "mirror" [ "v=" <> bvect v ] [build obj]

-- TODO(sandy): 'r' is not a rounding parameter
buildShared (Outset r obj) | r == 0 = call "outset" [] [build obj]

-- TODO(sandy): 'r' is not a rounding parameter
buildShared (Shell r obj) | r == 0 = call "shell" [] [build obj]

buildShared (WithRounding r obj) | r == 0 = build obj

buildShared(UnionR _ _) = error "cannot provide roundness when exporting openscad; unsupported in target format."
buildShared(IntersectR _ _) = error "cannot provide roundness when exporting openscad; unsupported in target format."
buildShared(DifferenceR _ _ _) = error "cannot provide roundness when exporting openscad; unsupported in target format."
buildShared(Outset _ _) = error "cannot provide roundness when exporting openscad; unsupported in target format."
buildShared(Shell _ _) = error "cannot provide roundness when exporting openscad; unsupported in target format."
buildShared(EmbedBoxedObj _) = error "cannot provide roundness when exporting openscad; unsupported in target format."
buildShared (WithRounding _ _) = error "cannot provide roundness when exporting openscad; unsupported in target format."


-- | First, the 3D objects.
buildS3 :: SymbolicObj3 -> Reader ℝ Builder

buildS3 (Shared3 obj) = buildShared obj

buildS3 (Cube (V3 w d h)) = call "cube" [bf w, bf d, bf h] []

buildS3 (Sphere r) = callNaked "sphere" ["r = " <> bf r] []

buildS3 (Cylinder h r1 r2) = callNaked "cylinder" [
                              "r1 = " <> bf r1
                             ,"r2 = " <> bf r2
                             , bf h
                             ] []
buildS3 (Rotate3 q obj) =
  let (x,y,z) = quaternionToEuler q
   in call "rotate" [bf (rad2deg x), bf (rad2deg y), bf (rad2deg z)] [buildS3 obj]

-- FIXME: where is EmbedBoxedObj3?

buildS3 (Extrude obj h) = callNaked "linear_extrude" ["height = " <> bf h] [buildS2 obj]

buildS3 (ExtrudeRotateR r twist obj h) | r == 0 = callNaked "linear_extrude" ["height = " <> bf h, "twist = " <> bf twist] [buildS2 obj]

-- FIXME: handle scale, center.
buildS3 (ExtrudeM twist scale (Left translate) obj (Left height)) |isScaleID scale && translate == V2 0 0 = do
  res <- ask
  let
    twist' = case twist of
               Left twval  -> const twval
               Right twfun -> twfun
  call "union" [] [
             call "rotate" ["0","0", bf $ twist' h] [
                        callNaked "linear_extrude" ["height = " <> bf res, "twist = " <> bf (twist' (h+res) - twist' h)][
                                   buildS2 obj
                                  ]
                       ] |  h <- take (floor (res / height)) $ fix (\f x -> x : f (x+res)) 0
            ]

-- FIXME: where are RotateExtrude, ExtrudeOnEdgeOf?

buildS3 ExtrudeRotateR {} = error "cannot provide roundness when exporting openscad; unsupported in target format."
buildS3 ExtrudeM{} = error "cannot provide roundness when exporting openscad; unsupported in target format."
buildS3 RotateExtrude{} = error "cannot provide roundness when exporting openscad; unsupported in target format."
buildS3(ExtrudeOnEdgeOf _ _) = error "cannot provide roundness when exporting openscad; unsupported in target format."

-- Now the 2D objects/transforms.

buildS2 :: SymbolicObj2 -> Reader ℝ Builder

buildS2 (Shared2 obj) = buildShared obj

buildS2 (Circle r) = call "circle" [bf r] []

buildS2 (Polygon points) = call "polygon" (fmap bvect points) []

buildS2 (Rotate2 r obj)     = call "rotate" [bf (rad2deg r)] [buildS2 obj]

-- Generate errors for rounding requests. OpenSCAD does not support rounding.
buildS2 Square{} = error "cannot provide roundness when exporting openscad; unsupported in target format."

-- FIXME: missing EmbedBoxedObj2?

