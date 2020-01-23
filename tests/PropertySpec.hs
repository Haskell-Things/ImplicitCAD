{-# LANGUAGE OverloadedStrings #-}

module PropertySpec
  ( propSpec
  ) where

import Prelude (($), Bool (True), (.))
import Test.Hspec (it, Spec)
import HaskellWorks.Hspec.Hedgehog (requireProperty)
import Hedgehog ((===))

propSpec :: Spec 
propSpec =
  it "trivial" . requireProperty $ do
    True === True
