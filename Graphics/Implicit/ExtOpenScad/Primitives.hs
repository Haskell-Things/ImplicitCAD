-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

-- We'd like to parse openscad code, with some improvements, for backwards compatability.

module Graphics.Implicit.ExtOpenScad.Primitives where

import Prelude hiding (lookup)
import Graphics.Implicit.Definitions
import qualified Graphics.Implicit.Primitives as Prim
import Graphics.Implicit.ExtOpenScad.Definitions
import Graphics.Implicit.ExtOpenScad.Expressions
import Graphics.Implicit.ExtOpenScad.Util
import Data.Map hiding (map,foldl,split,filter,null)
import Text.ParserCombinators.Parsec 
import Text.ParserCombinators.Parsec.Expr
import Control.Monad (liftM)

sphere = moduleWithoutSuite "sphere" $ do
	r <- realArgument "r";
	addObj3 $ Prim.sphere r;

cube = moduleWithoutSuite "cube" $ do
	size <- argument "size";
	center <- boolArgumentWithDefault "center" False;
	case size of
		OList ((ONum x):(ONum y):(ONum z):[]) -> 
			if center  
			then addObj3 $ Prim.cubeVC (x, y, z)
			else addObj3 $ Prim.cubeV (x, y, z)
		ONum w -> 
			if center
			then addObj3 $ Prim.cubeC w
			else addObj3 $ Prim.cube w
		_ -> noChange;

cylinder = moduleWithoutSuite "cylinder" $ do
	h  <- realArgumentWithDefault "h"  1;
	r  <- realArgumentWithDefault "r"  1;
	r1 <- realArgumentWithDefault "r1" 1;
	r2 <- realArgumentWithDefault "r2" 1;
	center <- boolArgumentWithDefault "center" False;
	if r1 == 1 && r2 == 1
	then 
		if center
		then addObj3 $ Prim.cylinderC r h
		else addObj3 $ Prim.cylinder  r h
	else
		if center
		then addObj3 $ Prim.cylinder2C r1 r2 h
		else addObj3 $ Prim.cylinder2  r1 r2 h




