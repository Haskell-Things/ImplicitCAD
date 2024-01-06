-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright (C) 2014-2017, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

module RewriteSpec (spec) where

import Prelude
  ( Bool(True)
  , Double
  , Eq((==))
  , Show
  , id
  , pure
  , ($)
  , (.)
  )

import qualified Test.Hspec
import Test.Hspec
  ( Expectation
  , Spec
  , describe
  , it
  )

import Linear
  ( V2(V2)
  , V3(V3)
  )

import Graphics.Implicit.Canon
  ( EqObj((=^=))
  , canonicalize2
  , canonicalize3
  , fmapObj2
  , fmapObj3
  , fmapSharedObj
  , rewriteUntilIrreducible
  )

import Graphics.Implicit.Definitions
  ( SharedObj(Translate)
  , SymbolicObj2(Square)
  , SymbolicObj3(Cube)
  )

import Graphics.Implicit.Primitives
  ( Object
  , circle
  , cube
  , emptySpace
  , extrude
  , fullSpace
  , implicit
  , pattern Shared
  , rotate
  , rotate3
  , scale
  , sphere
  , square
  , translate
  )

newtype WrapEq a = WrapEq a
  deriving Show

instance (EqObj a) => Eq (WrapEq a) where
  WrapEq a == WrapEq b = a =^= b

-- | shouldBe wrapper so we compare using EqObj
shouldBe :: (Show a, EqObj a) => a -> a -> Expectation
shouldBe a b = WrapEq a `Test.Hspec.shouldBe` WrapEq b

-- | Rewrite translations to scale
testRewShared :: Object obj f a => obj -> obj
testRewShared (Shared (Translate v o)) = scale v o
testRewShared x = x

sharedSample :: SymbolicObj2
sharedSample = translate 1 emptySpace

sharedExpected :: SymbolicObj2
sharedExpected = scale 1 fullSpace

-- | Rewrite squares to circles
testRew2 :: SymbolicObj2 -> SymbolicObj2
testRew2 (Square (V2 x _)) = circle x
testRew2 x = x

sym2Sample :: SymbolicObj2
sym2Sample =
  translate 1
    . rotate 3
    $ square True 1

sym2Expected :: SymbolicObj2
sym2Expected =
  scale 1
    . rotate 3.0
    . scale (-0.5)
    $ circle 1

-- | Rewrite cubes to spheres
testRew3 :: SymbolicObj3 -> SymbolicObj3
testRew3 (Cube (V3 x _ _)) = sphere x
testRew3 x = x

sym3Sample :: SymbolicObj3
sym3Sample =
  translate 1
    . rotate3 0
    $ cube True 10

sym3Expected :: SymbolicObj3
sym3Expected =
  scale 1
    . rotate3 0
    . scale (-5)
    $ sphere 10

sym32Sample :: SymbolicObj3
sym32Sample =
  translate 1
    . rotate3 0
    $ extrude 2 sym2Sample

sym32Expected :: SymbolicObj3
sym32Expected =
  scale 1
    . rotate3 0
    $ extrude 2 sym2Expected

spec :: Spec
spec =
  describe "fmap for objects" $ do
    describe "fmapSharedObj" $ do
      it "preserves identity" $
        fmapSharedObj id id sharedSample `shouldBe` sharedSample

      it "maps over tree" $
        fmapSharedObj (pure fullSpace) testRewShared sharedSample `shouldBe` sharedExpected

    describe "fmapObj2" $ do
      it "preserves identity" $
        fmapObj2 id id id sym2Sample `shouldBe` sym2Sample

      it "testRew2 id testRewShared" $
        fmapObj2 testRew2 id testRewShared sym2Sample `shouldBe` sym2Expected

    describe "fmapObj3" $ do
      it "identity" $
        fmapObj3 id id id sym3Sample `shouldBe` sym3Sample

      it "testRew3 id testRewShared" $
        fmapObj3 testRew3 id testRewShared sym3Sample `shouldBe` sym3Expected

      it "testRew3 testRew2 testRewShared" $
        fmapObj3 testRew3 testRew2 testRewShared sym32Sample `shouldBe` sym32Expected

    describe "rewriteUntilIrreducible" $ do
      describe "terminates" $ do
        it "simple" $
          rewriteUntilIrreducible id sym32Sample `shouldBe` sym32Sample

        it "handles implicit" $
          rewriteUntilIrreducible
            id
            (implicit @SymbolicObj2 @V2 @Double (\(V2 x _) -> x) (1, 1))
          `shouldBe` implicit (\(V2 x _) -> x) (1, 1)

    describe "canonicalize2" $ do
      let c2 = canonicalize2

      it "eliminates identities" $
        c2 (translate 0 $ rotate 0 $ circle 1) `shouldBe` circle 1

      it "eliminates identities after merging" $
        c2 (translate 1 $ scale 0 $ translate (-1) $ circle 1) `shouldBe` circle 1

    describe "canonicalize3" $ do
      let c3 = canonicalize3

      it "eliminates identities" $
        c3 (translate 0 $ scale 0 $ sphere 1) `shouldBe` sphere 1

      it "eliminates identities after merging" $
        c3 (translate 1 $ scale 0 $ translate (-1) $ sphere 1) `shouldBe` sphere 1

      it "handles 2D as well" $
        c3 (translate 1
             $ scale 0
             $ translate (-1)
             $ extrude 1
               $ translate 1
               $ scale 0
               $ translate (-1)
               $ circle 1
           ) `shouldBe` extrude 1 (circle 1)
