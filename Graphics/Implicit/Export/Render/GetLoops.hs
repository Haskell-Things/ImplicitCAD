-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

module Graphics.Implicit.Export.Render.GetLoops (getLoops) where

getLoops :: Eq a => [[a]] -> [[[a]]]
getLoops a = getLoops' a []

getLoops' :: Eq a => [[a]] -> [[a]] -> [[[a]]]

getLoops' [] [] = []

getLoops' (x:xs) [] = getLoops' xs [x]

getLoops' segs workingLoop | head (head workingLoop) == last (last workingLoop) =
	workingLoop : getLoops' segs []

getLoops' segs workingLoop =
	let
		presEnd = last $ last workingLoop
		connects (x:xs) = x == presEnd
		possibleConts = filter connects segs
		nonConts = filter (not . connects) segs
		(next, unused) = if null possibleConts
			then error "unclosed loop in paths given"
			else (head possibleConts, tail possibleConts ++ nonConts)
	in
		if null next
		then workingLoop : getLoops' segs []
		else getLoops' unused (workingLoop ++ [next])

