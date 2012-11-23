-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

module Graphics.Implicit.Export.Render.GetLoops (getLoops) where

import qualified Data.Sequence as SQ
import Data.Foldable (toList)

-- The goal of getLoops is, if you can imagine, extracting loops
-- from a list of segments.

-- The input is a list of segments
-- the output a list of loops, where each loop is a list of 
-- segments, which each piece representing a "side".

-- For example:
-- Given input [[1,2],[5,1],[3,4,5]] 
-- notice that there is a loop 1,2,3,4,5... <repeat>
-- But we give the output [ [1,2], [3,4,5], [5,1] ]
-- so that we have the loop, and also knowledge of how
-- the list is built (the "sides" of it).

getLoops :: Eq a => [[a]] -> [[[a]]]

-- We will be actually doing the loop extraction with
-- getLoops'

-- getLoops' has a first argument of the segments as before,
-- but a *second argument* which is the loop presently being
-- built.

-- so we begin with the "building loop" being empty. We use 
-- Data.Sequence for the building loop since we will often need
-- to refer to its last element.

getLoops a = getLoops' a SQ.empty


getLoops' :: Eq a => [[a]] -> SQ.Seq (SQ.Seq a) -> [[[a]]]

-- Obviously if there aren't any segments,
-- and the "building loop" is empty, 
-- we produce no loops.

getLoops' [] workingLoop | SQ.null workingLoop = []

-- And if the building loop is emtpy,
-- we stick the first segment we have onto it
-- to give us something to build on.

getLoops' (x:xs) workingLoop | SQ.null workingLoop =
    getLoops' xs $ SQ.singleton (SQ.fromList x)

-- A loop is finished if its start and end are the same.
-- In this case, we return it and empty the building loop.

getLoops' segs workingLoop | headSQ (headSQ workingLoop) == lastSQ (lastSQ workingLoop) =
	loop : getLoops' segs SQ.empty
    where loop = map toList (toList workingLoop)

-- Finally, we search for pieces that can continue the working loop,
-- and stick one on if we find it.
-- Otherwise... something is really screwed up.

getLoops' segs workingLoop =
	let
		presEnd = lastSQ $ lastSQ workingLoop
		connects (x:xs) = x == presEnd
		possibleConts = filter connects segs
		nonConts = filter (not . connects) segs
		(next, unused) = if null possibleConts
			then error "unclosed loop in paths given"
			else (head possibleConts, tail possibleConts ++ nonConts)
	in
		if null next
		then map toList (toList workingLoop) : getLoops' segs SQ.empty
		else getLoops' unused (workingLoop SQ.|> SQ.fromList next)

headSQ :: SQ.Seq a -> a
headSQ s = case SQ.viewl s of
               a SQ.:< _     -> a
               otherwise  -> error "headSQ: empty sequence"

lastSQ :: SQ.Seq a -> a
lastSQ s = case SQ.viewr s of
               _ SQ.:> a     -> a
               otherwise  -> error "lastSQ: empty sequence"
