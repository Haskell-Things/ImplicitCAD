-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

-- We'd like to parse openscad code, with some improvements, for backwards compatability.

module Graphics.Implicit.ExtOpenScad.Primitives where

import Prelude hiding (lookup)
import Graphics.Implicit.Definitions
import Graphics.Implicit.ExtOpenScad.Definitions
import Graphics.Implicit.ExtOpenScad.Expressions
import Graphics.Implicit.ExtOpenScad.Util
import Data.Map hiding (map,foldl,split,filter,null)
import Text.ParserCombinators.Parsec 
import Text.ParserCombinators.Parsec.Expr
import Control.Monad (liftM)

{-rootModule name argMapper producer = do
	string name
	pad $ char '('
	args <- possiblyNamedArgs
	pad $ char ')'
	return producer (argMap argMapper)-}






