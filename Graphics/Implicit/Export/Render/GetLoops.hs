-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

module Graphics.Implicit.Export.Render.GetLoops (getLoops) where


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

-- so we begin with the "building loop" being empty.

getLoops a = getLoops' a []


getLoops' :: Eq a => [[a]] -> [[a]] -> [[[a]]]

-- Obviously if there aren't any segments,
-- and the "building loop" is empty, 
-- we produce no loops.

getLoops' [] [] = []

-- And if the building loop is empty,
-- we stick the first segment we have onto it
-- to give us something to build on.

getLoops' (x:xs) [] = getLoops' xs [x]

-- A loop is finished if its start and end are the same.
-- In this case, we return it and empty the building loop.

getLoops' segs workingLoop | head (head workingLoop) == last (last workingLoop) =
    workingLoop : getLoops' segs []

-- Finally, we search for pieces that can continue the working loop,
-- and stick one on if we find it.
-- Otherwise... something is really screwed up.

-- FIXME: connects should be used with a singleton.

getLoops' segs workingLoop =
    let
        presEnd = last $ last workingLoop
        connects (x:_) = x == presEnd
        connects [] = False; -- silence compiler warning.
        possibleConts = filter connects segs
        nonConts = filter (not . connects) segs
        (next, unused) = if null possibleConts
            then error "unclosed loop in paths given"
            else (head possibleConts, tail possibleConts ++ nonConts)
    in
        if null next
        then workingLoop : getLoops' segs []
        else getLoops' unused (workingLoop ++ [next])

