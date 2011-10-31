-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

module Graphics.Implicit.Primitives (
	sphere,
	cube,
	circle,
	cylinder,
	square,
	regularPolygon,
	polygon,
	zsurface--,
	--ellipse
) where

import Graphics.Implicit.Definitions
import qualified Graphics.Implicit.SaneOperators as S

sphere :: 
	ℝ         -- ^ Radius of the sphere
	-> Obj3   -- ^ Resulting sphere
sphere r = \(x,y,z) -> sqrt (x**2 + y**2 + z**2) - r

cube :: 
	ℝ          -- ^ Width of the cube
	 -> Obj3   -- ^ Resuting cube
cube l = \(x,y,z) -> (maximum $ map abs [x,y,z]) - l/2.0

cylinder :: 
	ℝ         -- ^ Radius of the cylinder
	-> ℝ      -- ^ Height of the cylinder
	-> Obj3   -- ^ Resulting cylinder
cylinder r h = \(x,y,z) -> max (sqrt(x^2+y^2) - r) (abs(z) - h)

circle :: 
	ℝ        -- ^ radius of the circle
	-> Obj2  -- ^ resulting circle
circle r = \(x,y) -> sqrt (x**2 + y**2) - r

torus :: 
	ℝ       -- ^ radius of the rotated circle of a torus
	-> ℝ    -- ^ radius of the circle rotationaly extruded on of a torus
	-> Obj3 -- ^ resulting torus
torus r_main r_second = \(x,y,z) -> sqrt( ( sqrt (x^2 + y^2) - r_main )^2 + z^2 ) - r_second

--ellipse :: ℝ -> ℝ -> Obj2
--ellipse a b = \(x,y) ->
--	if a > b 
--	then ellipse b a (y,x)
--	else sqrt ((b/a*x)*	*2 + y**2) - a

square :: 
	ℝ        -- ^ Width of the square
	-> Obj2  -- ^ Resulting square
square l = \(x,y) -> (maximum $ map abs [x,y]) - l/2.0

polygon :: 
	[ℝ2]      -- ^ Verticies of the polygon
	 -> Obj2  -- ^ Resulting polygon
polygon points = 
	let
		pairs = 
		   [ (points !! n, points !! (mod (n+1) (length points) ) ) | n <- [0 .. (length points) - 1] ]
		isIn p@(p1,p2) = 
			let 
				crossing_points = 
					[x1 + (x2-x1)*y2/(y2-y1) |
					((x1,y1), (x2,y2)) <- 
						map (\((a1,a2),(b1,b2)) -> ((a1-p1,a2-p2), (b1-p1,b2-p2)) ) pairs,
					( (y2 < 0) && (y1 > 0) ) || ( (y2 > 0) && (y1 < 0) ) ]
			in 
				if odd $ length $ filter (>0) crossing_points then -1 else 1
		dist a@(a1,a2) b@(b1,b2) p@(p1,p2) =
			let
				ab = b S.- a
				nab = (1 / S.norm ab) S.* ab
				ap = p S.- a
				d  = nab S.⋅ ap
				closest 
					| d < 0 = a
					| d > S.norm ab = b
					| otherwise = a S.+ d S.* nab
			in
				S.norm (closest S.- p)
		dists = \ p -> map (\(a,b) ->  dist a b p) pairs
	in 
		\ p -> isIn p * minimum (dists p)

regularPolygon :: 
	ℕ       -- ^ number of sides
	-> ℝ    -- ^ radius
	-> Obj2 -- ^ resulting regular polygon
regularPolygon sides r = let sidesr = fromIntegral sides in
	\(x,y) -> maximum [ x*cos(2*pi*m/sidesr) + y*sin(2*pi*m/sidesr) | m <- [0.. sidesr -1]] - r 


zsurface :: 
	(ℝ2 -> ℝ) -- ^ Description of the height of the surface
	-> Obj3   -- ^ Resulting 3D object
zsurface f = \(x,y,z) -> f (x,y) - z


