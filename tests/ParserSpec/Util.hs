module ParserSpec.Util
       ( num
       , bool
       , app
       , app'
       , lambda'
       , parseWithEof
       , parseWithLeftOver
       , parseExpr
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
  | x < 0 = app' "negate" [LitE $ ONum (-x)]
  | otherwise = LitE $ ONum x

bool :: Bool -> Expr
bool = LitE . OBool

-- Operators and functions need two different kinds of applications
app :: String -> [Expr] -> Expr
app name args = Var name :$ [ListE args]

app' :: Symbol -> [Expr] -> Expr
app' name args = Var name :$ args

lambda' :: [Pattern] -> Expr -> [Expr] -> Expr
lambda' params expr args = LamE params expr :$ args

parseWithLeftOver :: Parser a -> String -> Either ParseError (a, String)
parseWithLeftOver p = parse ((,) <$> p <*> leftOver) ""
  where leftOver = manyTill anyToken eof

parseWithEof :: Parser a -> String -> String -> Either ParseError a
parseWithEof p = parse (p <* eof)

parseExpr :: String -> Either ParseError Expr
parseExpr = parseWithEof expr0 "expr"
