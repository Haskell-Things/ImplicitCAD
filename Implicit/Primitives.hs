-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

module Implicit.Primitives (
	sphere,
	cube,
	circle,
	cylinder,
	square,
	regularNGon,
	zsurface--,
	--ellipse
) where

import Implicit.Definitions


sphere :: ℝ -> Obj3
sphere r = \(x,y,z) -> sqrt (x**2 + y**2 + z**2) - r

cube :: ℝ -> Obj3
cube l = \(x,y,z) -> (maximum $ map abs [x,y,z]) - l/2

cylinder :: ℝ -> ℝ -> Obj3
cylinder r h = \(x,y,z) -> max (sqrt(x^2+y^2) - r) (abs(z) - h)

circle :: ℝ -> Obj2
circle r = \(x,y) -> sqrt (x**2 + y**2) - r

--ellipse :: ℝ -> ℝ -> Obj2
--ellipse a b = \(x,y) ->
--	if a > b 
--	then ellipse b a (y,x)
--	else sqrt ((b/a*x)*	*2 + y**2) - a

square :: ℝ -> Obj2
square l = \(x,y) -> (maximum $ map abs [x,y]) - l/2

roundRegularNGon sides n r =
	let unnormalized x y = (**(1/n)) $ sum $ map (**n) $ filter (>0) $
			[ x*cos(2*pi*m/sides) + y*sin(2*pi*m/sides) | m <- [0.. sides -1]] in 
	\(x,y) -> (unnormalized x y) / (unnormalized 1 0) - r 

regularNGon sides r =
	let unnormalized x y = maximum $
			[ x*cos(2*pi*m/sides) + y*sin(2*pi*m/sides) | m <- [0.. sides -1]] in 
	\(x,y) -> (unnormalized x y) / (unnormalized 1 0) - r 


zsurface :: (ℝ2 -> ℝ) -> Obj3
zsurface f = \(x,y,z) -> f (x,y) - z
