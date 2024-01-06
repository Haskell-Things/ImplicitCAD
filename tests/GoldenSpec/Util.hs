{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase   #-}

module GoldenSpec.Util (golden, goldenAllFormats, goldenFormat, goldenFormat2) where

import Control.Monad (forM_, unless)
import Control.Monad.IO.Class (liftIO)
import Graphics.Implicit (SymbolicObj2, SymbolicObj3)
import Graphics.Implicit.Export (export2, export3)
import Graphics.Implicit.Export.OutputFormat (OutputFormat (ASCIISTL), formats3D, formatExtension)
import Prelude (IO, FilePath, Bool (True, False), String, Double, pure, (==), (>>=), (<>), ($), show)
import System.Directory (getTemporaryDirectory, doesFileExist, removeFile)
import System.IO (hClose, openTempFile)
import Test.Hspec (describe, it, SpecWith)
import Test.HUnit (assertFailure)
import Data.ByteString (readFile, writeFile)

-- | Construct a golden test for rendering the given 'SymbolicObj3' at the
-- specified resolution in ASCIISTL format.
golden :: String -> Double -> SymbolicObj3 -> SpecWith ()
golden = goldenFormat ASCIISTL

-- | Construct a golden test for rendering the given 'SymbolicObj3' at the
-- specified resolution in all 3D formats.
goldenAllFormats :: String -> Double -> SymbolicObj3 -> SpecWith ()
goldenAllFormats name resolution sym = do
  describe ("golden " <> name)
    $ forM_ formats3D
    $ \fmt -> goldenFormat fmt name resolution sym

-- | Construct a golden test for rendering the given 'SymbolicObj2|3' at the
-- specified resolution. On the first run of this test, it will render the
-- object and cache the results. Subsequent test runs will compare their result
-- to the cached one. This is valuable for ensuring mesh generation doesn't
-- break across commits.
--
-- The objects are cached under @tests/golden/@, with the given name. Deleting
-- this file is sufficient to update the test if changes in the mesh generation
-- are intended.
goldenFormat'
  :: (   OutputFormat
      -> Double
      -> FilePath
      -> a
      -> IO ()
     )
  -> OutputFormat
  -> String
  -> Double
  -> a
  -> SpecWith ()
goldenFormat' exportFn fmt name resolution sym = it (name <> " (golden, format: " <> show fmt <> ")") $ do
  (okay, goldenFp, tempFp) <- liftIO $ do
    tempFp <- getTemporaryFilePath "golden"
    -- Output the rendered mesh
    exportFn fmt resolution tempFp sym
    !res <- readFile tempFp
    let goldenFp = "./tests/golden/" <> name <> "." <> formatExtension fmt
    -- Check if the cached results already exist.
    doesFileExist goldenFp >>= \case
      True  -> pure ()
      -- If not, save the mesh we just created in the cache.
      False -> writeFile goldenFp res
    !cached <- readFile goldenFp
    -- Finally, ceck if the two meshes are equal.
    if res == cached
      then do
        removeFile tempFp
        pure (True, goldenFp, tempFp)
      else
        pure (False, goldenFp, tempFp)

  unless okay
    $ assertFailure
    $ "Object doesn't match its golden preimage,"
    <> " temporary file preserved at "
    <> tempFp
    <> " compare with original at "
    <> goldenFp

-- | Test for @SymbolicObj3@
goldenFormat
  :: OutputFormat
  -> String
  -> Double
  -> SymbolicObj3
  -> SpecWith ()
goldenFormat = goldenFormat' export3

-- | Test for @SymbolicObj2@
goldenFormat2
  :: OutputFormat
  -> String
  -> Double
  -> SymbolicObj2
  -> SpecWith ()
goldenFormat2 = goldenFormat' export2

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
