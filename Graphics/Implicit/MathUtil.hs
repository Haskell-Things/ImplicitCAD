-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

module Graphics.Implicit.MathUtil (rmax, rmin, rmaximum, rminimum) where

import Data.List
import Graphics.Implicit.Definitions

-- | Rounded Maximum
-- Consider  max(x,y) = 0, the generated curve 
-- has a square-like corner. We replace it with a 
-- quarter of a circle
rmax :: 
	ℝ     -- ^ radius
	-> ℝ  -- ^ first number to round maximum
	-> ℝ  -- ^ second number to round maximum
	-> ℝ  -- ^ resulting number
rmax r x y =  if abs (x-y) < r 
	then y - r*sin(pi/4-asin((x-y)/r/sqrt 2)) + r
	else max x y

-- | Rounded minimum
rmin :: 
	ℝ     -- ^ radius
	-> ℝ  -- ^ first number to round minimum
	-> ℝ  -- ^ second number to round minimum
	-> ℝ  -- ^ resulting number
rmin r x y = if abs (x-y) < r 
	then y + r*sin(pi/4+asin((x-y)/r/sqrt 2)) - r
	else min x y

-- | Like rmax, but on a list instead of two.
-- Just as maximum is.
-- The implementation is to take the maximum two
-- and rmax those.

rmaximum ::
	ℝ      -- ^ radius
	-> [ℝ] -- ^ numbers to take round maximum
	-> ℝ   -- ^ resulting number
rmaximum _ (a:[]) = a
rmaximum r (a:b:[]) = rmax r a b
rmaximum r l = 
	let
		tops = reverse $ sort l
	in
		rmax r (tops !! 0) (tops !! 1)

-- | Like rmin but on a list.
rminimum ::
	ℝ      -- ^ radius
	-> [ℝ] -- ^ numbers to take round minimum
	-> ℝ   -- ^ resulting number
rminimum r (a:[]) = a
rminimum r (a:b:[]) = rmin r a b
rminimum r l = 
	let
		tops = sort l
	in
		rmin r (tops !! 0) (tops !! 1)

