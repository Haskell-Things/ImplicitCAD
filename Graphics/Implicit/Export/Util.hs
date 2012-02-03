-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts, TypeSynonymInstances, UndecidableInstances #-}

-- Functions to make meshes/polylines finer.

module Graphics.Implicit.Export.Util (divideMesh2To, divideMeshTo, dividePolylineTo) where

-- import Prelude hiding ((+),(-),(*),(/))
import Graphics.Implicit.Definitions
import qualified Graphics.Implicit.SaneOperators as S

-- If we need to make a 2D mesh finer...
divideMesh2To :: ℝ -> [(ℝ2, ℝ2, ℝ2)] -> [(ℝ2, ℝ2, ℝ2)]
divideMesh2To res mesh =
	let 
		av :: ℝ2 -> ℝ2 -> ℝ2
		av a b = (a S.+ b) S./ (2.0 :: ℝ)
		divideTriangle :: (ℝ2, ℝ2, ℝ2) -> [(ℝ2, ℝ2, ℝ2)]
		divideTriangle (a,b,c) =
			case (S.norm (a S.- b) > res, S.norm (b S.- c) > res, S.norm (c S.- a) > res) of
				(False, False, False) -> [(a,b,c)]
				(True,  False, False) -> [(a, av a b, c), 
				                          (av a b, b, c) ]
				(True,  True,  False) -> [(a, av a b, av a c), 
			                                  (av a b, b, av a c), 
				                          (b, c, av a c)]
				(True,  True,  True ) -> [(a, av a b, av a c), 
				                          (b, av b c, av b a), 
				                          (c, av c a, av c b),
				                          (av b c, av a c, av a b)]
				(_,_,_) -> divideTriangle (c, a, b)
	in
		concat $ map divideTriangle mesh

divideMeshTo :: ℝ -> [(ℝ3, ℝ3, ℝ3)] -> [(ℝ3, ℝ3, ℝ3)]
divideMeshTo res mesh =
	let 
		av :: ℝ3 -> ℝ3 -> ℝ3
		av a b = (a S.+ b) S./ (2.0 :: ℝ)
		divideTriangle :: (ℝ3, ℝ3, ℝ3) -> [(ℝ3, ℝ3, ℝ3)]
		divideTriangle (a,b,c) =
			case (S.norm (a S.- b) > res, S.norm (b S.- c) > res, S.norm (c S.- a) > res) of
				(False, False, False) -> [(a,b,c)]
				(True,  False, False) -> [(a, av a b, c), 
				                          (av a b, b, c) ]
				(True,  True,  False) -> [(a, av a b, av a c), 
			                                  (av a b, b, av a c), 
				                          (b, c, av a c)]
				(True,  True,  True ) -> [(a, av a b, av a c), 
				                          (b, av b c, av b a), 
				                          (c, av c a, av c b),
				                          (av b c, av a c, av a b)]
				(_,_,_) -> divideTriangle (c, a, b)
	in
		concat $ map divideTriangle mesh

dividePolylineTo :: ℝ -> [ℝ2] -> [ℝ2]
dividePolylineTo res polyline =
	let
		av :: ℝ2 -> ℝ2 -> ℝ2
		av a b = (a S.+ b) S./ (2.0 :: ℝ)
		divide a b = 
			if S.norm (a S.- b) <= res
			then [a]
			else concat [divide a (av a b), divide (av a b) b]
		n = length polyline
	in do
		m <- [0.. n]
		if m /= n
			then divide (polyline !! m) (polyline !! (m+1))
			else [polyline !! n]



