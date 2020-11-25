{-# LANGUAGE LambdaCase #-}

module GoldenSpec.Util (golden) where

import Control.Monad.IO.Class (liftIO)
import Graphics.Implicit (SymbolicObj3, writeSTL)
import Prelude (Bool (True, False), String, Double, pure, (==), readFile, writeFile, (>>=), (<>), ($))
import System.Directory (getTemporaryDirectory, doesFileExist)
import System.IO ( hClose )
import System.IO (openTempFile)
import Test.Hspec ( it, shouldBe, SpecWith )


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

