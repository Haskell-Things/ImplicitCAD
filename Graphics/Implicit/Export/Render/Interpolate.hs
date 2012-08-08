-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

module Graphics.Implicit.Export.Render.Interpolate (interpolate) where

interpolate (a,aval) (b,bval) _ _ | aval*bval > 0 = a
interpolate (a,aval) (b,bval) f res = 
	let
		a' = (a*95+5*b)/100
		b' = (b*95+5*a)/100
		a'val = f a'
		b'val = f b'
		deriva = abs $ 20*(aval - a'val)
		derivb = abs $ 20*(bval - b'val)
	in if deriva < 0.1 || derivb < 0.1
	then interpolate_bin 0 
		(if aval*a'val > 0 then (a',a'val) else (a,aval))
		(if bval*b'val > 0 then (b',b'val) else (b,bval))
		f
	else  interpolate_lin 0 
		(if aval*a'val > 0 then (a',a'val) else (a,aval))
		(if bval*b'val > 0 then (b',b'val) else (b,bval))
		f res

interpolate_lin _ (a, 0) _ _ _ = a
interpolate_lin _ _ (b, 0) _ _ = b
interpolate_lin n (a, aval) (b, bval) obj res | aval /= bval= 
	let
		mid = a + (b-a)*aval/(aval-bval)
		midval = obj mid
	in if abs midval < res/500 || mid > 3
	then mid
	else if midval * aval > 0
	then interpolate_lin (n+1) (mid, midval) (b, bval) obj res
	else interpolate_lin (n+1) (a,aval) (mid, midval) obj res
interpolate_lin _ (a, _) _ _ _ = a

interpolate_bin n (a,aval) (b,bval) f = if aval > bval
	then interpolate_bin' n (a,aval) (b,bval) f
	else interpolate_bin' n (b,bval) (a,aval) f

interpolate_bin' 4 (a,aval) (b,bval) f = 
	if abs aval < abs bval
	then a
	else b
interpolate_bin' n (a,aval) (b,bval) f =
	let
		mid = (a+b)/2
		midval = f mid
	in if midval > 0
	then interpolate_bin' (n+1) (mid,midval) (b,bval) f
	else interpolate_bin' (n+1) (a,aval) (mid,midval) f

