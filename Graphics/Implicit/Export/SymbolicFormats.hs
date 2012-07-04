-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

module Graphics.Implicit.Export.SymbolicFormats where

import Graphics.Implicit.Definitions
import Data.List as List

scad3 :: ℝ -> SymbolicObj3 -> String

scad3 res (UnionR3 0 objs) = 
	"union() {\n"
	++ concat (map ((++"\n") . scad3 res) objs)
	++ "}\n"
scad3 res (DifferenceR3 0 objs) = 
	"difference() {\n"
	++ concat (map ((++"\n") . scad3 res) objs)
	++ "}\n"
scad3 res (IntersectR3 0 objs) = 
	"intersection() {\n"
	++ concat (map ((++"\n") . scad3 res) objs)
	++ "}\n"
scad3 res (Translate3 (x,y,z) obj) =
	"translate ([" ++ show x ++ "," ++ show y ++ "," ++ show z ++ "]) "
	++ scad3 res obj
scad3 res (Scale3 (x,y,z) obj) =
	"scale ([" ++ show x ++ "," ++ show y ++ "," ++ show z ++ "]) "
	++ scad3 res obj
scad3 _ (Rect3R 0 (x1,y1,z1) (x2,y2,z2)) =
	"translate ([" ++ show x1 ++ "," ++ show y1 ++ "," ++ show z1 ++ "]) "
	++ "cube ([" ++ show (x2-x1) ++ "," ++ show (y2-y1) ++ "," ++ show (z2-z1) ++ "]);"
scad3 _ (Cylinder h r1 r2) =
	"cylinder(r1 = " ++ show r1 ++ ", r2 = " ++ show r2 ++ ", " ++ show h ++ ");"
scad3 _ (Sphere r) =
	"sphere(r = " ++ show r ++");"
scad3 res (ExtrudeR 0 obj h) =
	"linear_extrude(" ++ show h ++ ")"
	++ scad2 res obj
scad3 res (ExtrudeRotateR 0 twist obj h) = 
	"linear_extrude(" ++ show h ++ ", twist = " ++ show twist ++ " )"
	++ scad2 res obj
scad3 res (ExtrudeRM 0 (Just twist) Nothing Nothing obj (Left height)) =
	let
		for a b = map b a
		a ++! b = a ++ show b
	in (\pieces -> "union(){" ++ concat pieces ++ "}") . for (init [0, res.. height]) $ \h ->
		"rotate ([0,0," ++! twist h ++ "]) "
		++ "linear_extrude(" ++! res ++ ", twist = " ++! (twist (h+res) - twist h) ++ " )"
		++ scad2 res obj

scad2 res (UnionR2 0 objs) = 
	"union() {\n"
	++ concat (map ((++"\n") . scad2 res) objs)
	++ "}\n"
scad2 res (DifferenceR2 0 objs) = 
	"difference() {\n"
	++ concat (map ((++"\n") . scad2 res) objs)
	++ "}\n"
scad2 res (IntersectR2 0 objs) = 
	"intersection() {\n"
	++ concat (map ((++"\n") . scad2 res) objs)
	++ "}\n"
scad2 res (Translate2 (x,y) obj) =
	"translate ([" ++ show x ++ "," ++ show y ++ "," ++ "]) "
	++ scad2 res obj
scad2 res (Scale2 (x,y) obj) =
	"scale ([" ++ show x ++ "," ++ show y ++ "]) "
	++ scad2 res obj
scad2 _ (RectR 0 (x1,y1) (x2,y2)) =
	"translate ([" ++ show x1 ++ "," ++ show y1 ++ "]) "
	++ "cube ([" ++ show (x2-x1) ++ "," ++ show (y2-y1) ++ "]);"
scad2 _ (Circle r) = "circle(" ++ show r ++ ");"
scad2 _ (PolygonR 0 points) = 
	"polygon(" 
	++ "[" 
	++ (concat. List.intersperse "," . map (\(a,b) -> "["++show a++","++show b++"]" ) $ points)
	++ "]"
	++ ");"



