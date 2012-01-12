-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

module Graphics.Implicit.Primitives (
	sphere,
	cube, cubeC, cubeV, cubeVC,
	circle,
	cylinder, cylinderC, cylinder2, cylinder2C,
	square, squareC, squareV, squareVC,
	regularPolygon,
	polygon,
	zsurface
) where

import Graphics.Implicit.Definitions
import qualified Graphics.Implicit.SaneOperators as S

-- If you are confused as to how these functions work, please refer to
-- http://christopherolah.wordpress.com/2011/11/06/manipulation-of-implicit-functions-with-an-eye-on-cad/

sphere ::
	ℝ         -- ^ Radius of the sphere
	-> Obj3   -- ^ Resulting sphere
sphere r = \(x,y,z) -> sqrt (x**2 + y**2 + z**2) - r

cube ::
	ℝ          -- ^ Width of the cube
	 -> Obj3   -- ^ Resuting cube - (0,0,0) is bottom left...
cube l = \(x,y,z) -> (maximum $ map abs [x-l/2.0,y-l/2.0,z-l/2.0]) - l/2.0

cubeC ::
	ℝ          -- ^ Width of the cube
	 -> Obj3   -- ^ Resuting centered cube
cubeC l = \(x,y,z) -> (maximum $ map abs [x,y,z]) - l/2.0

cubeV ::
	ℝ3         -- ^ Dimensions of the cube
	 -> Obj3   -- ^ Resuting cube - (0,0,0) is bottom left...
cubeV (dx, dy, dz) = \(x,y,z) -> (maximum [abs (x-dx/2.0) - dx, abs (y-dy/2.0) - dy, abs (z-dz/2.0) - dz])

cubeVC ::
	ℝ3         -- ^ Dimensions of the cube
	 -> Obj3   -- ^ Resuting cube - (0,0,0) is bottom left...
cubeVC (dx, dy, dz) = \(x,y,z) -> (maximum [abs x - dx, abs y - dy, abs z - dz])


cylinder ::
	ℝ         -- ^ Radius of the cylinder	
	-> ℝ      -- ^ Height of the cylinder
	-> Obj3   -- ^ Resulting cylinder
cylinder r h = \(x,y,z) -> max (sqrt(x^2+y^2) - r) (abs(z-h/2.0) - h)

cylinderC ::
	ℝ         -- ^ Radius of the cylinder	
	-> ℝ      -- ^ Height of the cylinder
	-> Obj3   -- ^ Resulting cylinder
cylinderC r h = \(x,y,z) -> max (sqrt(x^2+y^2) - r) (abs(z) - h)

cylinder2 ::
	ℝ         -- ^ Radius of the cylinder	
	-> ℝ      -- ^ Second radius of the cylinder
	-> ℝ      -- ^ Height of the cylinder
	-> Obj3   -- ^ Resulting cylinder
cylinder2 r1 r2 h = \(x,y,z) -> max (sqrt(x^2+y^2) - r1*(1.0 - z/2.0) - r2*z/2.0) (abs(z-h/2.0) - h)

cylinder2C ::
	ℝ         -- ^ Radius of the cylinder	
	-> ℝ      -- ^ Second radius of the cylinder
	-> ℝ      -- ^ Height of the cylinder
	-> Obj3   -- ^ Resulting cylinder
cylinder2C r1 r2 h = \(x,y,z) -> max (sqrt(x^2+y^2) - r1*(1.0 - z/2.0) - r2*z/2.0) (abs(z) - h)


circle ::
	ℝ        -- ^ radius of the circle
	-> Obj2  -- ^ resulting circle
circle r = \(x,y) -> sqrt (x**2 + y**2) - r

torus ::
	ℝ       -- ^ radius of the rotated circle of a torus
	-> ℝ    -- ^ radius of the circle rotationaly extruded on of a torus
	-> Obj3 -- ^ resulting torus
torus r_main r_second = \(x,y,z) -> sqrt( ( sqrt (x^2 + y^2) - r_main )^2 + z^2 ) - r_second

-- This function is commented out because it doesn't obey the magnitude requirement.
-- Refer to blog post.
-- It needs to be fixed at some point, but the math is somewhat non-trivial.
--ellipse :: ℝ -> ℝ -> Obj2
--ellipse a b
--    | a < b = \(x,y) -> sqrt ((b/a*x)**2 + y**2) - a
--    | otherwise = \(x,y) -> sqrt (x**2 + (a/b*y)**2) - b

square ::
	ℝ        -- ^ Width of the square
	-> Obj2  -- ^ Resulting square (bottom corner = (0,0) )
square l = \(x,y) -> (maximum $ map abs [x- l/2.0,y- l/2.0]) - l/2.0

squareC ::
	ℝ        -- ^ Width of the square
	-> Obj2  -- ^ Resulting square (centered on (0,0))
squareC l = \(x,y) -> (maximum $ map abs [x,y]) - l/2.0

squareV ::
	ℝ2        -- ^ (x width, y width)
	-> Obj2  -- ^ Resulting square (bottom right = (0,0) )
squareV (dx,dy) = \(x,y) -> (maximum [abs (x- dx/2.0) -dx, abs (y- dy/2.0) - dy])

squareVC ::
	ℝ2        -- ^ Width of the square
	-> Obj2  -- ^ Resulting square
squareVC (dx,dy) = \(x,y) -> (maximum [abs (x-dx/2.0) - dx, abs (y-dy/2.0) - dy])



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


