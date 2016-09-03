-- recursively run a directory full of input files, saving the output somewhere useful.

import System.Directory
import System.IO
import Data.Char (toLower)

import System.FilePath.Posix

-- NOTE: make sure we don't import (<>) in new versions.
import Options.Applicative (fullDesc, progDesc, header, auto, info, helper, help, str, argument, switch,  long, short, option, metavar, execParser, Parser, optional, strOption)

-- Our Extended OpenScad interpreter, and functions to write out files in designated formats.
import Graphics.Implicit (runOpenscad, writeSVG, writeBinSTL, writeOBJ, writeSCAD2, writeSCAD3, writeGCodeHacklabLaser, writePNG2, writePNG3)
import Graphics.Implicit.ExtOpenScad.Definitions (LanguageOpts(LanguageOpts))

-- The following is needed to ensure backwards/forwards compatibility
-- Backwards compatibility with old versions of Data.Monoid:
infixr 6 <>
(<>) :: Monoid a => a -> a -> a
(<>) = mappend

data TestOptions = TestOptions
    { inputPath :: FilePath
    , outputPath :: FilePath
    }

-- The parser for our command line arguments.
testOptions :: Parser TestOptions
testOptions = TestOptions
    <$> argument str
        (  metavar "input-directory"
        <> help "Directory containing input source files."
        )
    <*> argument str
        (  metavar "output-directory"
        <> help "Directory in which to write output files."
        )

runFile :: FilePath -> FilePath -> FilePath -> IO ()
runFile inputPath fileName outputPath = do
    putStrLn $ "File: " ++ inputPath </> fileName ++ " " ++ baseName
    content <- readFile $ inputPath </> fileName
    case runOpenscad (LanguageOpts True True) content of
        Left err -> withFile (outputPath </> baseName <.> "err") WriteMode $ \h -> hPrint h err
        Right openscadProgram -> do
        return ()
    where baseName = fst $ splitExtension fileName
--    writeFile (outputPath </> fileName) contents

-- TODO fails on input with latin1 encoding (there is one in the OpenSCAD testdata.)
run :: TestOptions -> IO ()
run args = do
    putStrLn inputDir
    runDir inputDir
    return ()
    where
        runDir dir = do
            paths <- getDirectoryContents dir
            putStrLn dir
            runAll dir $ filter (`notElem` [".", ".."]) paths
        runAll _ [] = return ()
        runAll root (path:paths) = do
            runOne root path
            runAll root paths
        runOne root name = do
            let path = root </> name
            isFile <- doesFileExist path
            if isFile
            then if isScadFile name then runFile root name outputDir else return ()
            else runDir path
            where isScadFile path = case splitExtension $ map toLower path of
                                        (_, ".scad") -> True
                                        (_, ".escad") -> True
                                        _ -> False
        inputDir = inputPath args
        outputDir = outputPath args

main:: IO ()
main = execParser opts >>= run
    where
        opts = info (helper <*> testOptions)
              ( fullDesc
              <> progDesc "ImplicitCAD: Extended OpenSCAD interpreter."
              <> header "integrationtests - test by compiling all files in a directory."
              )
