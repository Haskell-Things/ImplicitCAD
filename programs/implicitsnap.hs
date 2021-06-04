{- ORMOLU_DISABLE -}
-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright (C) 2014 2015, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

-- FIXME: what are these for?
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

-- A Snap(HTTP) server providing an ImplicitCAD REST API.

-- FIXME: we need AuthN/AuthZ for https://github.com/kliment/explicitcad to be useful.

-- Let's be explicit about what we're getting from where :)

import Prelude (IO, Maybe(Just, Nothing), String, Bool(True, False), ($), (<>), (>), (.), (-), (/), (*), (**), (==), null, sqrt, min, max, minimum, maximum, show, return, fmap, otherwise, filter, not)

import Control.Applicative ((<|>))

import Snap.Core (Snap, route, writeText, writeBS, method, Method(GET), modifyResponse, setContentType, setTimeout, getRequest, rqParam)
import Snap.Http.Server (quickHttpServe)
import Snap.Util.GZip (withCompression)

-- Our Extended OpenScad interpreter, and the extrude function for making 2D objects 3D.
import Graphics.Implicit (runOpenscad, extrude, unionR)

-- Variable access functionality, so we can look up a requested resolution, along with message processing, and options into the scad engine.
import Graphics.Implicit.ExtOpenScad.Definitions (OVal(ONum), VarLookup, lookupVarIn, Message, Message(Message), MessageType(TextOut), ScadOpts(ScadOpts))

-- determine the size of boxes, so we can automagically create a resolution.
import Graphics.Implicit.Primitives (Object(getBox))

-- Definitions of the datatypes used for 2D objects, 3D objects, and for defining the resolution to raytrace at.
import Graphics.Implicit.Definitions (ℝ, Polyline, TriangleMesh, NormedTriangleMesh, SymbolicObj2, SymbolicObj3)

-- Use default values when a Maybe is Nothing.
import Data.Maybe (fromMaybe, maybe, fromJust)

import Graphics.Implicit.Export.TriangleMeshFormats (jsTHREE, stl)
import qualified Graphics.Implicit.Export.NormedTriangleMeshFormats as NTM (obj)

import Graphics.Implicit.Export.PolylineFormats (svg, dxf2, hacklabLaserGCode)

-- Operator for vector subtraction, to subtract two points. Used when defining the resolution of a 2d object.
import Linear.Affine((.-.))

-- class DiscreteApprox
import Graphics.Implicit.Export.DiscreteAproxable (discreteAprox)

import Data.List (intercalate)

import System.IO.Unsafe (unsafePerformIO)

import Data.ByteString.Char8 (unpack)
import Data.ByteString.UTF8 (fromString)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy as BSL (toStrict)
import Data.Text (Text, pack)
import Data.Text.Lazy as TL (toStrict)
import Data.Text.Encoding (encodeUtf8)
import Data.Aeson (encode)

-- To construct vectors of ℝs.
import Linear (V2(V2), V3(V3))

-- | The entry point. uses snap to serve a website.
main :: IO ()
main = quickHttpServe site

-- | Our site definition. Renders requests to "render/", discards all else.
site :: Snap ()
site = route
    [
        ("render/", renderHandler)
    ] <|> writeText "fall through"

-- | Our render/ handler. Uses source, callback, and opitional format to render an object.
renderHandler :: Snap ()
renderHandler = method GET $ withCompression $ do
    modifyResponse $ setContentType "application/x-javascript"
    setTimeout 600
    request <- getRequest
    case (rqParam "source" request, rqParam "callback" request, rqParam "format" request)  of
        (Just [source], Just [callback], Nothing) ->
            writeBS $ executeAndExport
                (unpack source)
                callback
                Nothing
        (Just [source], Just [callback], Just [format]) ->
            writeBS $ executeAndExport
                (unpack source)
                callback
                (Just format)
        (_, _, _)       -> writeText "must provide source and callback as 1 GET variable each"

-- | Find the resolution to raytrace at.
getRes :: (VarLookup, [SymbolicObj2], [SymbolicObj3], [Message]) -> ℝ
-- If specified, use a resolution specified by the "$res" a variable in the input file.
getRes (lookupVarIn "$res" -> Just (ONum res), _, _, _) = res
-- If there was no resolution specified, use a resolution chosen for 3D objects.
--   FIXME: magic numbers.
getRes (vars, _, obj:objs, _) =
    let
        (V3 x1 y1 z1, V3 x2 y2 z2) = getBox (unionR 0 (obj:objs))
        (V3 x y z) = V3 (x2-x1) (y2-y1) (z2-z1)
    in case fromMaybe (ONum 1) $ lookupVarIn "$quality" vars of
        ONum qual | qual > 0  -> min (minimum [x,y,z]/2) ((x*y*z/qual)**(1/3) / 22)
        _                     -> min (minimum [x,y,z]/2) ((x*y*z)**(1/3) / 22)
-- ... Or use a resolution chosen for 2D objects.
--   FIXME: magic numbers.
getRes (vars, obj:objs, _, _) =
    let
        (p1,p2) = getBox (unionR 0 (obj:objs))
        (V2 x y) = p2 .-. p1
    in case fromMaybe (ONum 1) $ lookupVarIn "$quality" vars of
        ONum qual | qual > 0 -> min (min x y/2) (sqrt(x*y/qual) / 30)
        _                    -> min (min x y/2) (sqrt(x*y) / 30)
-- fallthrough value.
getRes _ = 1

-- | get the maximum dimension of the object being rendered.
--   FIXME: shouldn't this get the diagonal across the box?
getWidth :: (VarLookup, [SymbolicObj2], [SymbolicObj3], [Message]) -> ℝ
getWidth (_,     _, obj:objs, _) = maximum [x2-x1, y2-y1, z2-z1]
    where (V3 x1 y1 z1, V3 x2 y2 z2) = getBox $ unionR 0 (obj:objs)
getWidth (_, obj:objs,     _, _) = max (x2-x1) (y2-y1)
    where (V2 x1 y1, V2 x2 y2) = getBox $ unionR 0 (obj:objs)
getWidth (_,    [],    [], _) = 0

formatIs2D :: Maybe ByteString -> Bool
formatIs2D (Just "SVG") = True
formatIs2D (Just "gcode/hacklab-laser") = True
formatIs2D _ = False

getOutputHandler2 :: ByteString -> [Polyline] -> Text
getOutputHandler2 name
  | name == "SVG"                   = TL.toStrict.svg
  | name == "gcode/hacklab-laser"   = TL.toStrict.hacklabLaserGCode
  | name == "DXF"                   = TL.toStrict.dxf2
  | otherwise                       = TL.toStrict.svg

formatIs3D :: Maybe ByteString -> Bool
formatIs3D (Just "STL") = True
formatIs3D (Just "OBJ") = True
formatIs3D _ = False

-- FIXME: OBJ support
getOutputHandler3 :: ByteString -> TriangleMesh -> Text
getOutputHandler3 name
  | name == "STL"                   = TL.toStrict.stl
  | otherwise                       = TL.toStrict.jsTHREE

getNormedOutputHandler3 :: ByteString -> NormedTriangleMesh -> Text
getNormedOutputHandler3 name
  | name == "OBJ"                   = TL.toStrict.NTM.obj
  | otherwise                       = TL.toStrict.NTM.obj

isTextOut :: Message -> Bool
isTextOut (Message TextOut _ _ ) = True
isTextOut _                      = False

-- | decide what options to send to the scad engine.
generateScadOpts :: ScadOpts
generateScadOpts = ScadOpts compat_flag import_flag
  where
    compat_flag = False -- Do not try to be extra compatible with openscad.
    import_flag = False -- Do not honor include or use statements.

-- | Give an openscad object to run and the basename of
--   the target to write to... write an object!
executeAndExport :: String -> ByteString -> Maybe ByteString -> ByteString
executeAndExport content callback maybeFormat =
    let
        showB :: Bool -> ByteString
        showB True  = "true"
        showB False = "false"
        showℝ :: ℝ -> ByteString
        showℝ val = fromString $ show val
        callbackF :: Bool -> Bool -> ℝ -> Text -> ByteString
        callbackF False is2D w msg =
            callback <> "([null," <> BSL.toStrict (encode msg) <> "," <> showB is2D <> "," <> showℝ w  <> "]);"
        callbackF True  is2D w msg =
            callback <> "([new Shape()," <> BSL.toStrict (encode msg) <> "," <> showB is2D <> "," <> showℝ w <> "]);"
        callbackS :: Text -> Text -> ByteString
        callbackS str          msg =
            callback <> "([" <> BSL.toStrict (encode str) <> "," <> BSL.toStrict (encode msg) <> ",null,null]);"
        scadOptions = generateScadOpts
        openscadProgram = runOpenscad scadOptions [] content
    in
      unsafePerformIO $ do
      s@(_, obj2s, obj3s, messages) <- openscadProgram
      let
        res = getRes   s
        w   = getWidth s
        emptyObject :: Text
        emptyObject = "Empty Object?"
                      <> " (No geometry generated -- whatever you did, the engine can make no sense of it)"
        render = res > 0
        scadMessages = pack $ intercalate "\n"
                       (fmap show (filter (not . isTextOut) messages) <>
                        fmap show (filter isTextOut messages))

      return $ case (obj2s, obj3s, render) of
        (_ ,        _, False) -> callbackF False False 1 emptyObject
        ([], obj:objs, _    ) -> do
          let target           = if null objs
                                 then obj
                                 else unionR 0 (obj:objs)
              unionWarning :: Text
              unionWarning     = if null objs
                                 then ""
                                 else " \nWARNING: Multiple objects detected. Adding a Union around them."
              output3d :: Text
              output3d         = if fromMaybe "jsTHREE" maybeFormat == "OBJ"
                                 then getNormedOutputHandler3 (fromJust maybeFormat) $ discreteAprox res target
                                 else maybe (TL.toStrict.jsTHREE) getOutputHandler3 maybeFormat $ discreteAprox res target
          if fromMaybe "jsTHREE" maybeFormat == "jsTHREE"
            then encodeUtf8 output3d <> callbackF True False w (scadMessages <> unionWarning)
            else if formatIs3D maybeFormat
                 then callbackS output3d (scadMessages <> unionWarning)
                 else callbackF False False 1 "Unable to render a 3D object into a 2D file."
        (obj:objs, []   , _) -> do
          let target          = if null objs
                                then obj
                                else unionR 0 (obj:objs)
              unionWarning :: Text
              unionWarning    = if null objs
                                then ""
                                else " \nWARNING: Multiple objects detected. Adding a Union around them."
              output3d        = maybe (TL.toStrict.jsTHREE) getOutputHandler3 maybeFormat $ discreteAprox res $ extrude target res
              output2d        = maybe (TL.toStrict.svg) getOutputHandler2 maybeFormat $ discreteAprox res target
          if fromMaybe "jsTHREE" maybeFormat == "jsTHREE"
            then encodeUtf8 output3d <> callbackF True True w (scadMessages <> unionWarning)
            else if formatIs2D maybeFormat
                 then callbackS output2d (scadMessages <> unionWarning)
                 else callbackS output3d (scadMessages <> unionWarning)
        ([], []         , _) -> callbackF False False 1 $ scadMessages <> "\n" <> "Nothing to render."
        _                    -> callbackF False False 1 $ scadMessages <> "\n" <> "ERROR: File contains a mixture of 2D and 3D objects, what do you want to render?"


