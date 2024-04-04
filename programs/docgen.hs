-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

-- FIXME: document why we need each of these.
{-# LANGUAGE ScopedTypeVariables  #-}

import Prelude(IO, Show, String, Int, Maybe(Just,Nothing), Eq, return, ($), show, fmap, (<>), putStrLn, filter, zip, null, undefined, const, Bool(True,False), fst, (.), head, length, (/=), (+), error, print, snd, (<$>))
import Data.Maybe (fromMaybe)
import Data.List (uncons)
import Graphics.Implicit.ExtOpenScad.Primitives (primitiveModules)
import Graphics.Implicit.ExtOpenScad.Definitions (ArgParser(AP,APFail,APExample,APTest,APTerminator,APBranch), Symbol(Symbol), OVal(ONModule), SourcePosition(SourcePosition), StateC)

import qualified Control.Exception as Ex (catch, SomeException)
import Control.Monad (forM_)
import Data.Traversable (traverse)
import Data.Text.Lazy (unpack, pack)

tail :: [a] -> [a]
tail l = fromMaybe [] $ snd <$> uncons l

-- | Return true if the argument is of type ExampleDoc.
isExample :: DocPart -> Bool
isExample (ExampleDoc _ ) = True
isExample _ = False

-- | Return true if the argument is of type ArgumentDoc.
isArgument :: DocPart -> Bool
isArgument (ArgumentDoc {}) = True
isArgument _ = False

-- | Return true if the argument is of type Branch.
isBranch :: DocPart -> Bool
isBranch (Branch _) = True
isBranch _ = False

dumpPrimitive :: Symbol -> [DocPart] -> Int -> IO ()
dumpPrimitive (Symbol moduleName) moduleDocList level = do
  let
    examples = filter isExample moduleDocList
    arguments = filter isArgument moduleDocList
    syntaxes = filter isBranch moduleDocList
    moduleLabel = unpack moduleName

  if level /= 0
    then
      putStrLn $ "#" <> moduleLabel
    else
    do
      putStrLn moduleLabel
      putStrLn (fmap (const '-') moduleLabel)
      putStrLn ""

  if null examples
    then
    return ()
    else
    do
      putStrLn "#Examples:\n"
      forM_ examples $ \x -> case x of
        (ExampleDoc example) -> putStrLn $ "   * `" <> example <> "`"
        _ -> error $ "Unexpected " <> show x <> " in examples"
      putStrLn ""

  if null arguments
    then
    return ()
    else
    do
      if level /= 0
        then
        putStrLn "##Arguments:\n"
        else
          if null syntaxes
          then
            putStrLn "#Arguments:\n"
          else
            putStrLn "#Shared Arguments:\n"
      forM_ arguments $ \x -> case x of
        (ArgumentDoc (Symbol name) posfallback description) ->
          case (posfallback, description) of
            (Nothing, "") ->
              putStrLn $ "   * `" <> unpack name  <> "`"
            (Just fallback, "") ->
              putStrLn $ "   * `" <> unpack name <> " = " <> fallback <> "`"
            (Nothing, _) -> do
              putStrLn $ "   * `" <> unpack name <> "`"
              putStrLn $ "     " <> description
            (Just fallback, _) -> do
              putStrLn $ "   * `" <> unpack name <> " = " <> fallback <> "`"
              putStrLn $ "     " <> description
        _ -> error $ "Unexpected " <> show x <> " in arguments"
      putStrLn ""

  if null syntaxes
    then
      return ()
    else
      forM_ syntaxes $ \x -> case x of
        (Branch syntax) ->
          dumpPrimitive (Symbol $ pack $ "Syntax " <> show (level+1)) syntax (level+1)
        _ -> error $ "Unexpected " <> show x <> " in syntaxes"

-- | Our entrypoint. Generate one document describing all of our primitives.
main :: IO ()
main = do
        docs <- traverse (getArgParserDocs.getArgParserFrom) primitiveModules
        let
          names = fmap fst primitiveModules
          docname = "ImplicitCAD Primitives"
        putStrLn (fmap (const '=') docname)
        putStrLn docname
        putStrLn (fmap (const '=') docname)
        putStrLn ""
        putStrLn ""
        forM_ (zip names docs) $ \(moduleName, moduleDocList) ->
          dumpPrimitive moduleName moduleDocList 0
          where
            getArgParserFrom :: (Symbol, OVal) -> ArgParser(StateC [OVal])
            getArgParserFrom (_, ONModule _ implementation _) = implementation sourcePosition []
              where sourcePosition = SourcePosition 0 0 "docgen"
            getArgParserFrom (_, _) = error "bad value in primitive array."

-- | the format we extract documentation into
data Doc = Doc String [DocPart]
             deriving (Show)

data DocPart = ExampleDoc String
             | ArgumentDoc Symbol (Maybe String) String
             | Branch [DocPart]
             | Empty
               deriving (Show, Eq)

--   Here there be dragons!
--   Because we made this a Monad instead of applicative functor, there's no sane way to do this.
--   We give undefined (= an error) and let laziness prevent if from ever being touched.
--   We're using IO so that we can catch an error if this backfires.
--   If so, we *back off*.

-- | Extract Documentation from an ArgParser
getArgParserDocs ::
    ArgParser a      -- ^ ArgParser(s)
    -> IO [DocPart]  -- ^ Docs (sadly IO wrapped)

getArgParserDocs (AP name fallback doc fnext) = do
  otherDocs <- Ex.catch (getArgParserDocs $ fnext undefined) (\(_ :: Ex.SomeException) -> return [])
  if otherDocs /= [Empty]
    then
          return $ ArgumentDoc name (fmap show fallback) (unpack doc) : otherDocs
    else
          return [ArgumentDoc name (fmap show fallback) (unpack doc)]

getArgParserDocs (APExample str child) = do
  childResults <- getArgParserDocs child
  return $ ExampleDoc (unpack str):childResults

-- We try to look at as little as possible, to avoid the risk of triggering an error.
-- Yay laziness!

getArgParserDocs (APTest _ _ child) = getArgParserDocs child

-- To look at these would almost certainly be death (exception)
getArgParserDocs (APTerminator _) = return [Empty]
getArgParserDocs (APFail _) = return [Empty]

-- This one confuses me.
getArgParserDocs (APBranch children) = do
  print (length children)
  otherDocs <- Ex.catch (getArgParserDocs (APBranch $ tail children)) (\(_ :: Ex.SomeException) -> return [])
  aResults <- getArgParserDocs $ head children
  if otherDocs /= [Empty]
    then
    return [Branch (aResults <> otherDocs)]
    else
    return aResults
