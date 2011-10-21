-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

module Implicit.MathUtil (rmax, rmin) where

-- Rounded Maximum
-- Consider  max(x,y) = 0, the generated curve 
-- has a square-like corner. We replace it with a bevel.

rmax r x y =  if abs (x-y) < r 
	then y - r*sin(pi/4-asin((x-y)/r/sqrt 2)) + r
	else max x y

-- Rounded minimum

rmin r x y = if abs (x-y) < r 
	then y + r*sin(pi/4+asin((x-y)/r/sqrt 2)) - r
	else min x y



