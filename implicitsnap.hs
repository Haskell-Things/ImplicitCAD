{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

{- This is a Snap server providing a ImplicitCAD REST API.
   It does not install by default. Its dependencies are not in the cabal file.
   We're just sticking it in the repo for lack of a better place... -}

module Main where

import Control.Applicative
import Snap.Core
import Snap.Http.Server
import Snap.Util.GZip (withCompression)

import Graphics.Implicit (runOpenscad)
import Graphics.Implicit.ExtOpenScad.Definitions (OpenscadObj (ONum))
import Graphics.Implicit.ObjectUtil (getBox2, getBox3)
import Graphics.Implicit.Export.TriangleMeshFormats (jsTHREE)
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
	case (rqParam "source" request, rqParam "callback" request)  of
		(Just [source], Just [callback]) -> do
			writeBS $ BS.Char.pack $ executeAndExport 
				(BS.Char.unpack source)
				(BS.Char.unpack callback)
		(_, _)       -> writeBS "must provide source and callback as 1 GET variable each"
 


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
			if defaultRes <= 4*requestedRes
			then requestedRes
			else -1
		_ -> 
			if qual <= 8
			then qualRes
			else -1




-- | Give an openscad object to run and the basename of 
--   the target to write to... write an object!
executeAndExport :: String -> String -> String
executeAndExport content callback = 
	let
		callbackF :: Bool -> String -> String
		callbackF False msg = callback ++ "([null," ++ show msg ++ "]);"
		callbackF True  msg = callback ++ "([new Shape()," ++ show msg ++ "]);"
	in case runOpenscad content of
		Left err -> 
			let
				line = sourceLine . errorPos $ err
				showErrorMessages' = showErrorMessages 
					"or" "unknown parse error" "expecting" "unexpected" "end of input"
				msgs :: String
				msgs = showErrorMessages' $ errorMessages err
			in callbackF False $ (\s-> "error (" ++ show line ++ "):" ++ s) msgs
		Right openscadProgram -> unsafePerformIO $ do 
			s <- openscadProgram 
			let
				res = getRes s
			return $ case s of 
				(_, _, x:xs)  -> 
					if res > 0
					then TL.unpack (jsTHREE (discreteAprox res x)) ++ callbackF True ""
					else callbackF False $ 
						"Unreasonable resolution requested: "
						++ "the server imps revolt! " 
						++ "(Install ImplicitCAD locally -- github.com/colah/ImplicitCAD/)"
				_ ->  callbackF False "not a 3D object"


