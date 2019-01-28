-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright (C) 2014 2015, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

-- Allow us to use explicit foralls when writing function type declarations.
{-# LANGUAGE ExplicitForAll #-}

{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE ViewPatterns #-}

-- A Snap(HTTP) server providing an ImplicitCAD REST API.

-- Let's be explicit about what we're getting from where :)

import Prelude (IO, Maybe(Just, Nothing), Ord, String, Bool(True, False), Either(Left, Right), Show, ($), (++), (>), (.), (-), (/), (*), (**), sqrt, min, max, minimum, maximum, show, return)

import Control.Applicative ((<|>))

import Snap.Core (Snap, route, writeBS, method, Method(GET), modifyResponse, setContentType, setTimeout, getRequest, rqParam)
import Snap.Http.Server (quickHttpServe)
import Snap.Util.GZip (withCompression)

-- Our Extended OpenScad interpreter, and the extrudeR function for making 2D objects 3D.
import Graphics.Implicit (runOpenscad, extrudeR)

import Graphics.Implicit.ExtOpenScad.Definitions (OVal (ONum))

-- Functions for finding a box around an object, so we can define the area we need to raytrace inside of.
import Graphics.Implicit.ObjectUtil (getBox2, getBox3)

-- Definitions of the datatypes used for 2D objects, 3D objects, and for defining the resolution to raytrace at.
import Graphics.Implicit.Definitions (SymbolicObj2, SymbolicObj3, ℝ)

-- Use default values when a Maybe is Nothing.
import Data.Maybe (fromMaybe)

import Graphics.Implicit.Export.TriangleMeshFormats (jsTHREE, stl)
import Graphics.Implicit.Export.PolylineFormats (svg, hacklabLaserGCode)

-- Operator to subtract two points. Used when defining the resolution of a 2d object.
import Data.AffineSpace ((.-.))

-- class DiscreteApprox
import Graphics.Implicit.Export.DiscreteAproxable (discreteAprox)

import Data.String (IsString)
import Data.Map.Strict as Map (lookup, Map)

import Text.ParserCombinators.Parsec (errorPos, sourceLine)
import Text.ParserCombinators.Parsec.Error (errorMessages, showErrorMessages)

import System.IO.Unsafe (unsafePerformIO)
import System.IO.Silently (capture)

import qualified Data.ByteString.Char8 as BS.Char (pack, unpack)
import qualified Data.Text.Lazy as TL (unpack)

main :: IO ()
main = quickHttpServe site

site :: Snap ()
site = route
    [
        ("render/", renderHandler)
    ] <|> writeBS "fall through"

renderHandler :: Snap ()
renderHandler = method GET $ withCompression $ do
    modifyResponse $ setContentType "application/x-javascript"
    setTimeout 600
    request <- getRequest
    case (rqParam "source" request, rqParam "callback" request, rqParam "format" request)  of
        (Just [source], Just [callback], Nothing) ->
            writeBS . BS.Char.pack $ executeAndExport
                (BS.Char.unpack source)
                (BS.Char.unpack callback)
                Nothing
        (Just [source], Just [callback], Just [format]) ->
            writeBS . BS.Char.pack $ executeAndExport
                (BS.Char.unpack source)
                (BS.Char.unpack callback)
                (Just $ BS.Char.unpack format)
        (_, _, _)       -> writeBS "must provide source and callback as 1 GET variable each"

-- Find the resolution to raytrace at.
getRes :: forall k. (Data.String.IsString k, Ord k) => (Map k OVal, [SymbolicObj2], [SymbolicObj3]) -> ℝ

-- First, use a resolution specified by a variable in the input file.
getRes (Map.lookup "$res" -> Just (ONum res), _, _) = res

-- If there was no resolution specified, use a resolution chosen for 3D objects.
-- FIXME: magic numbers.
getRes (varlookup, _, obj:_) =
    let
        ((x1,y1,z1),(x2,y2,z2)) = getBox3 obj
        (x,y,z) = (x2-x1, y2-y1, z2-z1)
    in case fromMaybe (ONum 1) $ Map.lookup "$quality" varlookup of
        ONum qual | qual > 0  -> min (minimum [x,y,z]/2) ((x*y*z/qual)**(1/3) / 22)
        _                     -> min (minimum [x,y,z]/2) ((x*y*z     )**(1/3) / 22)
-- Use a resolution chosen for 2D objects.
-- FIXME: magic numbers.
getRes (varlookup, obj:_, _) =
    let
        (p1,p2) = getBox2 obj
        (x,y) = p2 .-. p1
    in case fromMaybe (ONum 1) $ Map.lookup "$quality" varlookup of
        ONum qual | qual > 0 -> min (min x y/2) (sqrt(x*y/qual) / 30)
        _                    -> min (min x y/2) (sqrt(x*y     ) / 30)
-- fallthrough value.
getRes _ = 1

{-
getRes (varlookup, obj2s, obj3s) =
    let
        qual = case Map.lookup "$quality" varlookup of
            Just (ONum n) | n >= 1  ->  n
            _                       ->  1
        (defaultRes, qualRes) = case (obj2s, obj3s) of
            (_, obj:_) -> ( min (minimum [x,y,z]/2) ((x*y*z     )**(1/3) / 22)
                          , min (minimum [x,y,z]/2) ((x*y*z/qual)**(1/3) / 22))
                where
                    ((x1,y1,z1),(x2,y2,z2)) = getBox3 obj
                    (x,y,z) = (x2-x1, y2-y1, z2-z1)
            (obj:_, _) -> ( min (min x y/2) (sqrt(x*y     ) / 30)
                          , min (min x y/2) (sqrt(x*y/qual) / 30) )
                where
                    ((x1,y1),(x2,y2)) = getBox2 obj
                    (x,y) = (x2-x1, y2-y1)
            _ -> (1, 1)
    in case Map.lookup "$res" varlookup of
        Just (ONum requestedRes) ->
            if defaultRes <= 30*requestedRes
            then requestedRes
            else -1
        _ ->
            if qual <= 30
            then qualRes
            else -1
-}

getWidth :: forall t. (t, [SymbolicObj2], [SymbolicObj3]) -> ℝ
getWidth (_,     _, obj:_) = maximum [x2-x1, y2-y1, z2-z1]
    where ((x1,y1,z1),(x2,y2,z2)) = getBox3 obj
getWidth (_, obj:_,     _) = max (x2-x1) (y2-y1)
    where ((x1,y1),(x2,y2)) = getBox2 obj
getWidth (_, [], []) = 0

-- | Give an openscad object to run and the basename of
--   the target to write to... write an object!
executeAndExport :: String -> String -> Maybe String -> String
executeAndExport content callback maybeFormat =
    let
        showB :: IsString t => Bool -> t
        showB True  = "true"
        showB False = "false"
        callbackF :: Bool -> Bool -> ℝ -> String -> String
        callbackF False is2D w msg =
            callback ++ "([null," ++ show msg ++ "," ++ showB is2D ++ "," ++ show w  ++ "]);"
        callbackF True  is2D w msg =
            callback ++ "([new Shape()," ++ show msg ++ "," ++ showB is2D ++ "," ++ show w ++ "]);"
        callbackS :: (Show a1, Show a) => a -> a1 -> String
        callbackS str   msg = callback ++ "([" ++ show str ++ "," ++ show msg ++ ",null,null]);"
    in case runOpenscad content of
        Left err ->
            let
                line = sourceLine . errorPos $ err
                showErrorMessages' = showErrorMessages
                    "or" "unknown parse error" "expecting" "unexpected" "end of input"
                msgs :: String
                msgs = showErrorMessages' $ errorMessages err
            in callbackF False False 1 $ (\s-> "error (" ++ show line ++ "):" ++ s) msgs
        Right openscadProgram -> unsafePerformIO $ do
            (msgs,s) <- capture openscadProgram
            let
                res = getRes   s
                w   = getWidth s
                is2D = case s of
                    (_, _, _:_)  -> False
                    (_, _:_, _)  -> True
                    _             -> False
                highResError = "Unreasonable resolution requested: "
                            ++ "the server imps revolt! "
                            ++ "(Install ImplicitCAD locally -- github.com/colah/ImplicitCAD/)"
                objOrErr = case s of
                    (_, _, x:_)  ->
                        if res > 0
                        then Right (Nothing, x)
                        else Left highResError
                    (_, x:_, _) ->
                        if res > 0
                        then Right (Just x, extrudeR 0 x res)
                        else Left highResError
                    _            ->  Left $ msgs ++ "Nothing to render."

            return $ case (objOrErr, maybeFormat) of
                (Left errmsg, _) -> callbackF False False 1 errmsg
                (Right (_,obj), Nothing)  ->
                    TL.unpack (jsTHREE (discreteAprox res obj)) ++ callbackF True is2D w msgs
                (Right (_,obj), Just "STL") ->
                    callbackS (TL.unpack (stl (discreteAprox res obj))) msgs
                (Right (Just obj, _), Just "SVG") ->
                    callbackS (TL.unpack (svg (discreteAprox res obj))) msgs
                (Right (Just obj, _), Just "gcode/hacklab-laser") ->
                    callbackS (TL.unpack (hacklabLaserGCode (discreteAprox res obj))) msgs
                (Right (_ , _), _) ->
                    callbackF False False 1 "unexpected case"


