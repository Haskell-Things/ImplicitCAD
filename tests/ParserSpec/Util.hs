module ParserSpec.Util
       ( num
       , bool
       , app
       , app'
       , plus
       , minus
       , divide
       , modulo
       , power
       , mult
       , parseWithLeftOver
       ) where

import Graphics.Implicit.Definitions
import Graphics.Implicit.ExtOpenScad
import Graphics.Implicit.ExtOpenScad.Definitions
import Graphics.Implicit.ExtOpenScad.Parser.Expr
import Text.Parsec.String
import Text.Parsec.Error
import Text.ParserCombinators.Parsec  hiding (State)
import Control.Applicative ((<$>), (<*>), (<*), (*>))

num :: ℝ -> Expr
num x
  -- note that the parser should handle negative number literals
  -- directly, we abstract that deficiency away here
  | x < 0 = app "negate" [LitE $ ONum (-x)]
  | otherwise = LitE $ ONum x

bool :: Bool -> Expr
bool = LitE . OBool

-- Operators and functions need two different kinds of applications
app, app' :: String -> [Expr] -> Expr
app name args = Var name :$ args
app' name args = Var name :$ [ListE args]

plus, minus, mult, modulo, power :: [Expr] -> Expr
plus = app' "+"
minus = app "-"
mult = app' "*"
modulo = app "%"
power = app "^"
divide = app "/"

parseWithLeftOver :: Parser a -> String -> Either ParseError (a, String)
parseWithLeftOver p = parse ((,) <$> p <*> leftOver) ""
  where leftOver = manyTill anyToken eof
