{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE TupleSections    #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns     #-}

module TesselationSpec (spec) where

import Prelude
import Test.Hspec
    (shouldSatisfy,  hspec, describe, shouldBe, shouldContain, Spec, Expectation )
import Test.QuickCheck
import Data.Foldable ( for_ )
import Test.Hspec.QuickCheck (prop)
import Data.List (sortBy, nub, sort, group)
import Data.Traversable ( for )
import Graphics.Implicit.Export.Render.GetLoops (getLoops)
import Graphics.Implicit.Test.Utils (randomGroups)
import Graphics.Implicit.Test.Instances ()
import Control.Monad (when,  join )
import Control.Lens (view,  Ixed(ix), (&), (.~) )
import Linear
import Graphics.Implicit.Export.Render.TesselateLoops (tesselateLoop)
import Graphics.Implicit.Definitions (Triangle(..), ℝ3, Obj3, TriangleMesh(..))
import Graphics.Implicit.Export.Render.Definitions (TriSquare(Tris))
import Debug.Trace (traceM)
import Data.Functor.Identity
import GHC.Generics (Generic)
import Graphics.Implicit.Export.Render.Interpolate (interpolate, interpolateBin, test)

spec :: Spec
spec = do
  describe "getLoops" $ do
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

    prop "inserting in the middle is ok" $ do
      (_, segs) <- genLoop @Int 0
      let n = length segs
      -- Pick a random segment
      seg_idx <- choose (0, n - 1)
      -- Insert a random element into it
      seg' <- insertMiddle (segs !! seg_idx) =<< arbitrary
      let segs' = segs & ix seg_idx .~ seg'

      pure $ do
        -- We should be able to get the loops of the original and inserted segments.
        Just [loop] <- pure $ getLoops segs
        Just [loop'] <- pure $ getLoops segs'
        -- Really we're just testing to make sure the above pattern match doesn't
        -- 'fail', but let's make sure they have the same number of segments too.
        length loop `shouldBe` length loop'

    describe "interpolate" $ do
      prop "should find roots" $ withMaxSuccess 100000 $
            \(functionalize -> f) -> do
        res <- choose (0.001, 5)
        let mkVal x = V2 x (f x)
            aval = mkVal (-res)
            bval = mkVal res
        -- make sure our randomly chosen bounds have different signs
        !_ <- when (view _y aval * view _y bval > 0) discard

        let x' = interpolate aval bval f res

        pure $ counterexample (show res) $ counterexample ("x^-1 = " <> show x') $ do
          -- make sure our generated function has a root
          f 0 `shouldBe` 0

          -- see if interpolate can find the root
          f x' `shouldSatisfy` (< 10) . abs




--   describe "tesselateLoop" $ do
--     prop "should preserve normals for points in a plane" $
--           \(Positive res) (NonEmpty (nub -> ps)) -> do
--       let q = Quaternion 1 0 :: Quaternion Double
--       -- Make sure we have enough points
--       !_ <- when (length ps < 3) discard

--       -- Take our R2 points, give them a Z component, and then rotate them by
--       -- the quaternion. The result is a point on our plane.
--       let points = fmap (rotate q . mkV3 0) ps

--       -- Generate random segments for our points
--       segs <- randomGroups points

--       -- The normal for our plane is the Z vector rotated by the same quaternion.
--       let normal = rotate q $ V3 0 0 1
--       pure $ do
--         counterexample (show segs) $ do
--         counterexample (show normal) $ do
--           [Tris (TriangleMesh triangles)] <- pure @Maybe $ tesselateLoop res (plane normal) (loopify segs)
--           pure $ do
--             counterexample (show triangles) $ do
--               -- Compute the normal for each triangle. Check that each one of them
--               -- dotted against the normal is >0.99.
--               let normals = fmap (dot normal . triangleNormal) triangles
--               normals `shouldSatisfy` (all $ (> 0.99))


triangleNormal :: Triangle -> ℝ3
triangleNormal (Triangle (a, b, c)) = normalize $ (b - a) `cross` (c - a)

plane :: ℝ3 -> Obj3
plane (normalize -> n) = dot n



mkV3 :: Num a => a -> V2 a -> V3 a
mkV3 a v2 = pure 0 & _xy .~ v2 & _z .~ a


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


------------------------------------------------------------------------------
-- | Insert an element into the middle (not 'head' or 'last') of a list.
insertMiddle :: [a] -> a -> Gen [a]
insertMiddle [] _ = pure []
insertMiddle [a] _ = pure [a]
insertMiddle as a = do
  let n = length as
  i <- choose (1, n - 1)
  pure $ insertAt i a as


------------------------------------------------------------------------------
-- | Helper function to insert an element into a list at a given position.
--
-- Stolen from https://hackage.haskell.org/package/ilist-0.4.0.1/docs/Data-List-Index.html#v:insertAt
insertAt :: Int -> a -> [a] -> [a]
insertAt i a ls
  | i < 0 = ls
  | otherwise = go i ls
  where
    go 0 xs     = a : xs
    go n (x:xs) = x : go (n-1) xs
    go _ []     = []



data Defunc
  = Linear Double  -- ^ Slope
  | Sinusoid
      Double  -- ^ amplitude
      Double  -- ^ frequency
  | Edge
      Double  -- ^ amplitude
  deriving (Eq, Ord, Show, Generic)

functionalize :: Defunc -> Double -> Double
functionalize (Linear slope) = \r -> r * slope
functionalize (Sinusoid amp freq) = \r -> amp * sin (freq * r)
functionalize (Edge amp) = \r -> amp * signum r


instance Arbitrary Defunc where
  shrink = genericShrink
  arbitrary = sized $ \n ->
    case n <= 1 of
      True  -> oneof small
      False -> oneof $ []  ++ small
    where
      small =
        [ Linear <$> arbitrary
        , Sinusoid <$> arbitrary <*> arbitrary
        , Edge <$> arbitrary
        ]

