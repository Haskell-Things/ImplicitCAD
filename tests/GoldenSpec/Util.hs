{- ORMOLU_DISABLE -}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase   #-}

module GoldenSpec.Util (golden) where

import Control.Monad.IO.Class (liftIO)
import Graphics.Implicit (SymbolicObj3, writeSTL)
import Prelude (IO, FilePath, Bool (True, False), String, Double, pure, (==), readFile, writeFile, (>>=), (<>), ($))
import System.Directory (getTemporaryDirectory, doesFileExist)
import System.IO (hClose, openTempFile)
import Test.Hspec (it, shouldBe, SpecWith)

------------------------------------------------------------------------------
-- | Construct a golden test for rendering the given 'SymbolicObj3' at the
-- specified resolution. On the first run of this test, it will render the
-- object and cache the results. Subsequent test runs will compare their result
-- to the cached one. This is valuable for ensuring mesh generation doesn't
-- break across commits.
--
-- The objects are cached under @tests/golden/@, with the given name. Deleting
-- this file is sufficient to update the test if changs in the mesh generation
-- are intended.
golden :: String -> Double -> SymbolicObj3 -> SpecWith ()
golden name resolution sym = it (name <> " (golden)") $ do
  (res, cached) <- liftIO $ do
    temp_fp <- getTemporaryFilePath "stl"
    -- Output the rendered mesh
    writeSTL resolution temp_fp sym
    !res <- readFile temp_fp
    let golden_fp = "./tests/golden/" <> name <> ".stl"
    -- Check if the cached results already exist.
    doesFileExist golden_fp >>= \case
      True  -> pure ()
      -- If not, save the mesh we just created in the cache.
      False -> writeFile golden_fp res
    !cached <- readFile golden_fp
    pure (res, cached)
  -- Finally, ceck if the two meshes are equal.
  if res == cached
    then pure ()
    else False `shouldBe` True

------------------------------------------------------------------------------
-- | Get a temporary filepath with the desired extension. On unix systems, this
-- is a file under @/tmp@. Useful for tests that need to write files.
getTemporaryFilePath
    :: String  -- ^ File extension
    -> IO FilePath
getTemporaryFilePath ext = do
  tempdir <- getTemporaryDirectory
  -- The only means available to us for getting a temporary filename also opens
  -- its file handle. Because the 'writeSTL' function opens the file handle
  -- itself, we must first close our handle.
  (fp, h) <- openTempFile tempdir "implicit-golden"
  hClose h
  pure $ fp <> "." <> ext

