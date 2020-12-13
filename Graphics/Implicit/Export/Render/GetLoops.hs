-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright 2014 2015 2016, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

{-# LANGUAGE ScopedTypeVariables #-}

module Graphics.Implicit.Export.Render.GetLoops (getLoops) where

-- Explicitly include what we want from Prelude.
import Prelude (not, ($), head, last, tail, (==), Bool(False), (.), null, error, (<>), Show, Eq)
import Data.Maybe
import Control.Applicative

import Data.List (partition)
import Control.Monad

-- | The goal of getLoops is to extract loops from a list of segments.
--   The input is a list of segments.
--   The output a list of loops, where each loop is a list of
--   segments, which each piece representing a "side".

-- For example:
-- Given points [[1,2],[5,1],[3,4,5],[2,3], ... ]
-- notice that by going from 1->2 in the first segment, we can then go 2->3 via the fourth.
-- In this sense, there is a loop 1,2,3,4,5... <repeat>
-- But we give the output [ [ [1,2], [2,3], [3,4,5], [5,1] ], ... ]
-- so that we have the loop, and also knowledge of how
-- the list is built (the "sides" of it).

getLoops :: (Show a, Eq a) => [[a]] -> [[[a]]]
getLoops a = fromMaybe (error "unclosed loop in paths given") $
  getLoops' a []

-- We will be actually doing the loop extraction with
-- getLoops'

-- getLoops' has a first argument of the segments as before,
-- but a *second argument* which is the loop presently being
-- built.

-- so we begin with the "building loop" being empty.
getLoops'
    :: forall a
     . (Show a, Eq a)
    => [[a]]
    -> [[a]]  -- ^ accumulator
    -> Maybe [[[a]]]

-- | If there aren't any segments, and the "building loop" is empty, produce no loops.
getLoops' [] [] = Just []

-- | If the building loop is empty, stick the first segment we have onto it to give us something to build on.
getLoops' ([]:xs) [] = getLoops' xs []
getLoops' (x:xs) [] = getLoops' xs [x]

-- | A loop is finished if its start and end are the same.
-- Return it and start searching for another loop.

-- NOTE(sandy): This @last . last@ is atrocious and runs in @O(n^2)@ due to the
-- recursion here. It's *especially* bad since this is in the tight @O(n^3)@
-- loop of 'Graphics.Implicit.Export.Render.getMesh'.
getLoops' segs workingLoop | head (head workingLoop) == last (last workingLoop) =
  (workingLoop :) <$> getLoops' segs []

-- Finally, we search for pieces that can continue the working loop,
-- and stick one on if we find it.
-- Otherwise... something is really screwed up.
getLoops' segs workingLoop = do
    let
        presEnd :: [[a]] -> a
        presEnd = last . last

        connects :: [a] -> Bool
        connects (x:_) = x == presEnd workingLoop
        connects [] = False

        -- divide our set into sequences that connect, and sequences that don't.
        possibleConts, nonConts :: [[a]]
        (possibleConts, nonConts) = partition connects segs

    case possibleConts of
      [] -> empty
      (next : conts) -> do
        let unused = conts <> nonConts
        if null next
          then (workingLoop :) <$> getLoops' segs []
          else getLoops' unused (workingLoop <> [next])

