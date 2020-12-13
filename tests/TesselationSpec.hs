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


proveLoop :: (Show a, Eq a) => [a] -> [[a]] -> Expectation
proveLoop v loops =
  join (replicate 2 v) `shouldContain` unsegment loops


genLoop :: Enum a => a -> Gen ([a], [[a]])
genLoop start = do
  n <- getPositive <$> arbitrary @(Positive Int)
  let v = take n $ enumFrom start
  bits <- randomGroups v
  let segs = mkSegments bits
  shuffled_segs <- shuffle segs
  pure (v, shuffled_segs)


genManyLoops :: Enum a => a -> Int -> Gen ([[(Int, a)]], [[[(Int, a)]]])
genManyLoops start n = do
  fmap unzip $ for [0 .. n - 1] $ \idx -> do
    -- Generate a loop for each
    (v, segs) <- genLoop start
    -- and tag it with the index
    pure (fmap (idx,) v, fmap (fmap (idx,)) segs)



mkSegments :: [[a]] -> [[a]]
mkSegments as = zipWith (\a -> mappend a . take 1) as $ drop 1 $ join $ repeat as

unsegment :: Eq a => [[a]] -> [a]
unsegment = drop 1 . fmap head . group . join

