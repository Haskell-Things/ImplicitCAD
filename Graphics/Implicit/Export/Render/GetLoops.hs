-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright 2014 2015 2016, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

module Graphics.Implicit.Export.Render.GetLoops (getLoops) where

-- Explicitly include what we want from Prelude.
import Prelude (head, last, tail, (==), Bool(False), (.), null, error, (<>))

-- We're working with 3D points here.
import Graphics.Implicit.Definitions (ℝ3)

import Data.List (partition)

-- | The goal of getLoops is to extract loops from a list of segments.
--   The input is a list of segments.
--   The output a list of loops, where each loop is a list of 
--   segments, which each piece representing a "side".

-- For example:
-- Given points [[1,2],[5,1],[3,4,5], ... ] 
-- notice that there is a loop 1,2,3,4,5... <repeat>
-- But we give the output [ [ [1,2], [3,4,5], [5,1] ], ... ]
-- so that we have the loop, and also knowledge of how
-- the list is built (the "sides" of it).

getLoops :: [[ℝ3]] -> [[[ℝ3]]]
getLoops a = getLoops' a []

-- We will be actually doing the loop extraction with
-- getLoops'

-- getLoops' has a first argument of the segments as before,
-- but a *second argument* which is the loop presently being
-- built.

-- so we begin with the "building loop" being empty.
getLoops' :: [[ℝ3]] -> [[ℝ3]] -> [[[ℝ3]]]

-- | If there aren't any segments, and the "building loop" is empty, produce no loops.
getLoops' [] [] = []

-- | If the building loop is empty, stick the first segment we have onto it to give us something to build on.
getLoops' (x:xs) [] = getLoops' xs [x]

-- | A loop is finished if its start and end are the same.
-- Return it and start searching for another loop.
getLoops' segs workingLoop | head (head workingLoop) == last (last workingLoop) =
    workingLoop : getLoops' segs []

-- Finally, we search for pieces that can continue the working loop,
-- and stick one on if we find it.
-- Otherwise... something is really screwed up.
getLoops' segs workingLoop =
    let
        presEnd :: [[ℝ3]] -> ℝ3
        presEnd = last . last
        connects (x:_) = x == presEnd workingLoop
        connects [] = False -- Handle the empty case.
        -- divide our set into sequences that connect, and sequences that don't.
        (possibleConts, nonConts) = partition connects segs
        (next, unused) = if null possibleConts
            then error "unclosed loop in paths given"
            else (head possibleConts, tail possibleConts <> nonConts)
    in
        if null next
        then workingLoop : getLoops' segs []
        else getLoops' unused (workingLoop <> [next])

