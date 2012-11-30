{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

-- PACKAGES: snap, silently

{- This is a Snap server providing a ImplicitCAD REST API.
   It does not install by default. Its dependencies are not in the cabal file.
   We're just sticking it in the repo for lack of a better place... -}

module Main where

import Control.Applicative
import Snap.Core
import Snap.Http.Server
import Snap.Util.GZip (withCompression)

import Graphics.Implicit (runOpenscad, extrudeR)
import Graphics.Implicit.ExtOpenScad.Definitions (OVal (ONum))
import Graphics.Implicit.ObjectUtil (getBox2, getBox3)
import Graphics.Implicit.Export.TriangleMeshFormats (jsTHREE, stl)
import Graphics.Implicit.Export.PolylineFormats (svg, hacklabLaserGCode)
import Graphics.Implicit.Definitions (xmlErrorOn, errorMessage)
import Data.Map as Map
import Text.ParserCombinators.Parsec (errorPos, sourceLine)
import Text.ParserCombinators.Parsec.Error

-- class DiscreteApproxable
import Graphics.Implicit.Export.Definitions

-- instances of DiscreteApproxable...
import Graphics.Implicit.Export.SymbolicObj2
import Graphics.Implicit.Export.SymbolicObj3

import System.IO.Unsafe (unsafePerformIO)
import System.IO.Silently (capture)

import qualified Data.ByteString.Char8 as BS.Char
import qualified Data.Text.Lazy as TL

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
	request <- getRequest
	case (rqParam "source" request, rqParam "callback" request, rqParam "format" request)  of
		(Just [source], Just [callback], Nothing) -> do
			writeBS $ BS.Char.pack $ executeAndExport 
				(BS.Char.unpack source)
				(BS.Char.unpack callback)
				Nothing
		(Just [source], Just [callback], Just [format]) -> do
			writeBS $ BS.Char.pack $ executeAndExport 
				(BS.Char.unpack source)
				(BS.Char.unpack callback)
				(Just $ BS.Char.unpack format)
		(_, _, _)       -> writeBS "must provide source and callback as 1 GET variable each"
 


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
			(obj:_, _) -> ( min (min x y/2) ((x*y     )**0.5 / 30)
			              , min (min x y/2) ((x*y/qual)**0.5 / 30) )
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


getWidth (varlookup,     _, obj:_) = maximum [x2-x1, y2-y1, z2-z1]
	where ((x1,y1,z1),(x2,y2,z2)) = getBox3 obj
getWidth (varlookup, obj:_,     _) = max (x2-x1) (y2-y1)
	where ((x1,y1),(x2,y2)) = getBox2 obj


-- | Give an openscad object to run and the basename of 
--   the target to write to... write an object!
executeAndExport :: String -> String -> Maybe String -> String
executeAndExport content callback maybeFormat = 
	let
		showB True  = "true"
		showB False = "false"
		callbackF :: Bool -> Bool -> Float -> String -> String
		callbackF False is2D w msg = 
			callback ++ "([null," ++ show msg ++ "," ++ showB is2D ++ "," ++ show w  ++ "]);"
		callbackF True  is2D w msg = 
			callback ++ "([new Shape()," ++ show msg ++ "," ++ showB is2D ++ "," ++ show w ++ "]);"
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
			(msgs,s) <- capture $ openscadProgram 
			let
				res = getRes   s
				w   = getWidth s
				is2D = case s of
					(_, _, x:xs)  -> False
					(_, x:xs, _)  -> True
					_             -> False
				highResError = "Unreasonable resolution requested: "
							++ "the server imps revolt! " 
							++ "(Install ImplicitCAD locally -- github.com/colah/ImplicitCAD/)"
				objOrErr = case s of
					(_, _, x:xs)  -> 
						if res > 0 
						then Right (Nothing, x)
						else Left highResError
					(_, x:xs, _) -> 
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



