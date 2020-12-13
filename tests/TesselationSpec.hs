{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module TesselationSpec (spec) where

import Prelude
import Test.Hspec
    ( hspec, describe, shouldBe, shouldContain, Spec, Expectation )
import Test.QuickCheck
    ( Arbitrary(arbitrary),
      choose,
      shuffle,
      Gen,
      Positive(getPositive) )
import Data.Foldable ( for_ )
import Test.Hspec.QuickCheck (prop)
import Data.List (sort, group)
import Data.Traversable ( for )
import Graphics.Implicit.Export.Render.GetLoops (getLoops)
import Graphics.Implicit.Test.Utils (randomGroups)
import Control.Monad ( join )

spec :: Spec
spec = describe "getLoops" $ do
  prop "stability" $ do
    n <- choose (2, 20)
    (_, segs) <- genManyLoops @Int 0 n
    -- Shuffle the loops amongst themselves (but dont intermingle their segments)
    shuffled_segs <- shuffle segs
    pure $ do
      Just loops <- pure $ getLoops $ join shuffled_segs
      -- The discovered loops should be in the same order that we generated
      -- them in
      for_ (zip loops shuffled_segs) $ \(loop, seg) ->
        head loop `shouldBe` head seg

  prop "loops a loop" $ do
    (v, segs) <- genLoop @Int 0
    pure $ do
      Just [loop] <- pure $ getLoops segs
      proveLoop v loop

  prop "loops many loops" $ do
    -- Pick a number of loops to aim for
    n <- choose (2, 20)
    (vs, segs) <- genManyLoops @Int 0 n

    -- Shuffle the segments of all the loops together
    shuffled_segs <- shuffle $ join segs
    pure $ do
      Just loops <- pure $ getLoops shuffled_segs
      -- Make sure we have the right length
      length loops `shouldBe` n
      -- Ensure that we can 'proveLoop' on each loop
      for_ (zip vs $ sort loops) $ uncurry proveLoop


------------------------------------------------------------------------------
-- | Show that the given loop exists somewhere in the discovered loops.
-- Correctly deals with the case where the two loops start at different places.
proveLoop :: (Show a, Eq a) => [a] -> [[a]] -> Expectation
proveLoop v loops =
  join (replicate 2 v) `shouldContain` unloop loops


------------------------------------------------------------------------------
-- | Generate a loop and random segments that should produce it. The defining
-- equation of this generator is tested by "getLoops > loops a loop".
genLoop
    :: Enum a
    => a
    -> Gen ([a], [[a]])  -- ^ @(loop, segments)@
genLoop start = do
  n <- getPositive <$> arbitrary @(Positive Int)
  let v = take n $ enumFrom start
  bits <- randomGroups v
  let segs = loopify bits
  shuffled_segs <- shuffle segs
  pure (v, shuffled_segs)


------------------------------------------------------------------------------
-- | Like 'genLoop', but produces several loops, tagged with an index number.
-- For best results, you should call @shuffle . join@ on the resulting segments
-- before calling @getLoops@ on it, to ensure the segments are intermingled
-- between the loops.
genManyLoops
    :: Enum a
    => a
    -> Int  -- ^ Number of loops to generate
    -> Gen ([[(Int, a)]], [[[(Int, a)]]])  -- ^ @(loop, segments)@
genManyLoops start n = do
  fmap unzip $ for [0 .. n - 1] $ \idx -> do
    -- Generate a loop for each
    (v, segs) <- genLoop start
    -- and tag it with the index
    pure (fmap (idx,) v, fmap (fmap (idx,)) segs)


------------------------------------------------------------------------------
-- | Given a list of lists, insert elements into the 'head' and 'last' of each
-- sub-list so that the 'last' of one list is the 'head' of the next.
loopify :: [[a]] -> [[a]]
loopify as = zipWith (\a -> mappend a . take 1) as $ drop 1 $ join $ repeat as

------------------------------------------------------------------------------
-- | Remove sequential elements in a list. Additionally, this function removes
-- the 'head' of the list, because conceptully it is also the 'last'.
unloop :: Eq a => [[a]] -> [a]
unloop = drop 1 . fmap head . group . join

