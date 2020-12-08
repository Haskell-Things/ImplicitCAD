-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright (C) 2016 Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

-- FIXME: describe why we need this.
{-# LANGUAGE OverloadedStrings #-}

module Graphics.Implicit.Export.NormedTriangleMeshFormats (obj) where

import Prelude(($), fmap, (+), (.), (*), length, (-), pure, (<>))

import Graphics.Implicit.Definitions (NormedTriangle(NormedTriangle), NormedTriangleMesh(NormedTriangleMesh), ℝ3)
import Graphics.Implicit.Export.TextBuilderUtils (Text, Builder, toLazyText, bf, buildInt)

import Data.Foldable (fold, foldMap)
import Linear (V3(V3))

-- | Generate a .obj format file from a NormedTriangleMesh
--   see: https://en.wikipedia.org/wiki/Wavefront_.obj_file
obj :: NormedTriangleMesh -> Text
obj (NormedTriangleMesh normedtriangles) = toLazyText $ vertcode <> normcode <> trianglecode
    where
        -- | A vertex line; v (0.0, 0.0, 1.0) = "v 0.0 0.0 1.0\n"
        v :: ℝ3 -> Builder
        v (V3 x y z) = "v "  <> bf x <> " " <> bf y <> " " <> bf z <> "\n"
        -- | A normal line; n (0.0, 0.0, 1.0) = "vn 0.0 0.0 1.0\n"
        n :: ℝ3 -> Builder
        n (V3 x y z) = "vn " <> bf x <> " " <> bf y <> " " <> bf z <> "\n"
        verts = do
            --  Extract the vertices for each triangle.
            --  recall that a normed triangle is of the form ((vert, norm), ...)
            NormedTriangle ((a,_),(b,_),(c,_)) <- normedtriangles
            -- The vertices from each triangle take up 3 positions in the resulting list
            [a,b,c]
        norms = do
            -- extract the normals for each triangle
            NormedTriangle ((_,a),(_,b),(_,c)) <- normedtriangles
            -- The normals from each triangle take up 3 positions in the resulting list
            [a,b,c]
        vertcode = foldMap v verts
        normcode = foldMap n norms
        trianglecode :: Builder
        trianglecode = fold $ do
            n' <- fmap ((+1).(*3)) [0,1 .. length normedtriangles -1]
            let
                vta = buildInt  n'
                vtb = buildInt (n'+1)
                vtc = buildInt (n'+2)
            pure $ "f " <> vta <> " " <> vtb <> " " <> vtc <> " " <> "\n"

