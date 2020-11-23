{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}

module GoldenTestSpec (spec) where

import Control.Monad.IO.Class (liftIO)
import Graphics.Implicit
import Prelude
import System.Directory (getTemporaryDirectory, doesFileExist)
import System.IO ( hClose )
import System.IO (openTempFile)
import Test.Hspec ( describe, it, shouldBe, Spec, SpecWith )


spec :: Spec
spec = describe "golden tests" $ do
  golden 1 "box" $ cubeR 0 True (5, 5, 5)


golden :: Double -> String -> SymbolicObj3 -> SpecWith ()
golden resolution name sym = it (name <> " (golden)") $ do
  (res, cached) <- liftIO $ do
    tempdir <- getTemporaryDirectory
    (fp, h) <- openTempFile tempdir "implicit-golden"
    hClose h
    writeSTL resolution fp sym
    res <- readFile fp
    let golden_fp = "./tests/golden/" <> name <> ".stl"
    doesFileExist golden_fp >>= \case
      True  -> pure ()
      False -> writeFile golden_fp res
    cached <- readFile golden_fp
    pure (res, cached)
  case res == cached of
    True -> pure ()
    False -> False `shouldBe` True

