{-# LANGUAGE OverloadedStrings #-}

-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

module Graphics.Implicit.Export.NormedTriangleMeshFormats where

import Graphics.Implicit.Definitions
import Graphics.Implicit.Export.TextBuilderUtils


obj :: [NormedTriangle] -> Text
obj normedtriangles = toLazyText $ vertcode <> normcode <> trianglecode
    where
        -- A vertex line; v (0.0, 0.0, 1.0) = "v 0.0 0.0 1.0\n"
        v :: ℝ3 -> Builder
        v (x,y,z) = "v "  <> bf x <> " " <> bf y <> " " <> bf z <> "\n"
        -- A normal line; n (0.0, 0.0, 1.0) = "vn 0.0 0.0 1.0\n"
        n :: ℝ3 -> Builder
        n (x,y,z) = "vn " <> bf x <> " " <> bf y <> " " <> bf z <> "\n"
        verts = do
            -- extract the vertices for each triangle
            -- recall that a normed triangle is of the form ((vert, norm), ...)
            ((a,_),(b,_),(c,_)) <- normedtriangles
            -- The vertices from each triangle take up 3 position in the resulting list
            [a,b,c]
        norms = do
            -- extract the normals for each triangle
            ((_,a),(_,b),(_,c)) <- normedtriangles
            -- The normals from each triangle take up 3 position in the resulting list
            [a,b,c]
        vertcode = mconcat $ map v verts
        normcode = mconcat $ map n norms
        trianglecode = mconcat $ do
            n <- map ((+1).(*3)) [0,1 .. length normedtriangles -1]
            let
                vta = buildInt  n
                vtb = buildInt (n+1)
                vtc = buildInt (n+2)
            return $ "f " <> vta <> " " <> vtb <> " " <> vtc <> " " <> "\n"

