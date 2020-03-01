-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright (C) 2014-2019 , Julia Longtin (julial@turinglace.com)
-- Copyright (C) 2016, Kelvin Cookshaw (kelvin@cookshaw.com)
-- Released under the GNU AGPLV3+, see LICENSE

-- Allow us to use shorter forms of Var and Name.
{-# LANGUAGE PatternSynonyms #-}

-- A parser for a numeric expressions.
module Graphics.Implicit.ExtOpenScad.Parser.Expr(expr0) where

import Prelude (Char, Maybe(Nothing, Just), String, ($), (<>), id, foldl, foldr, (==), length, head, (&&), (<$>), (<*>), (*>), (<*), flip, (.), pure)

import Graphics.Implicit.ExtOpenScad.Definitions (Expr(LamE, LitE, ListE, (:$)), OVal(ONum, OUndefined), Symbol(Symbol))

import qualified Graphics.Implicit.ExtOpenScad.Definitions as GIED (Expr(Var), Pattern(Name))

import Graphics.Implicit.ExtOpenScad.Parser.Util ((?:), (*<|>), number, boolean, scadString, scadUndefined, variable)

-- The lexer.
import Graphics.Implicit.ExtOpenScad.Parser.Lexer (whiteSpace, matchLet, matchTok, matchColon, matchComma, surroundedBy, matchIdentifier, matchEQ, matchNE, matchLE, matchLT, matchGE, matchGT, matchCAT, matchAND, matchOR)

-- The parsec parsing library.
import Text.Parsec (oneOf, many, sepBy, optionMaybe, option, (<|>), chainl1, chainr1)

import Text.Parsec.String (GenParser)

import Control.Monad.Fix(fix)

-- Let us use the old syntax when defining Vars and Names.
pattern Var :: String -> Expr
pattern Var  s = GIED.Var  (Symbol s)
pattern Name :: String -> GIED.Pattern
pattern Name n = GIED.Name (Symbol n)

-- Borrowed the pattern from http://compgroups.net/comp.lang.functional/parsing-ternary-operator-with-parsec/1052460
-- In the levels list, the first element is the lowest precedent, and the last is the highest.
-- "higher" represents the higher precedence parser, ie. the next one in the levels list.
-- "fix $ \self ->..." is used to consume all expressions in the same level, "self" being the current level.
expr0 :: GenParser Char st Expr
expr0 = foldr ($) nonAssociativeExpr levels
  where
    levels :: [GenParser Char st Expr -> GenParser Char st Expr]
    levels =
      [ id
      , \higher -> fix $ \self -> do -- ?: ternary operator.
          condition <- higher
          do
            trueExpr  <- matchTok '?' *> self
            falseExpr <- matchColon   *> self
            pure $ Var "?" :$ [condition, trueExpr, falseExpr]
           <|>
            pure condition
      , \higher -> -- boolean OR operator (||)
          chainl1 higher $ binaryOperation <$> matchOR
      , \higher -> -- boolean AND operator (&&)
          chainl1 higher $ binaryOperation <$> matchAND
      , \higher -> -- == and != operators
          chainl1 higher $ binaryOperation <$> (matchEQ <|> matchNE)
      , \higher -> -- <, <=, >= and > operators
          chainl1 higher $ binaryOperation <$> (matchLE <|> matchLT <|> matchGE <|> matchGT)
      , \higher -> -- + and - operators
          chainl1 higher $ binaryOperation . pure <$> oneOf "+-" <* whiteSpace
      , \higher -> -- string/list concatenation operator (++). This is not available in OpenSCAD.
          chainl1 higher $ binaryOperation <$> matchCAT
      , \higher -> -- exponent operator (^). This is not available in OpenSCAD.
          chainr1 higher $ binaryOperation <$> matchTok '^'
      , \higher -> -- multiplication (*), division (/), and modulus (%) operators
          chainl1 higher $ binaryOperation . pure <$> oneOf "*/%" <* whiteSpace
      , \higher ->
          fix $ \self -> -- unary ! operator. OpenSCAD's YACC parser puts '!' at the same level of precedence as '-' and '+'.
                  do
                    op <- matchTok '!'
                    right <- self
                    -- when noting a not, just skip both of them.
                    pure $ case right of
                      Var "!" :$ [deepright] -> deepright
                      _                      -> Var op :$ [right]
        <|>
          higher
      , \higher -> -- leading positive or negative sign.
          fix $ \self ->
              do -- Unary -. applied to strings is undefined, but handle that in the interpreter.
                right <- matchTok '-' *> self
                pure $ Var "negate" :$ [right]
          <|> do -- Unary +. Handle this by ignoring the +
                matchTok '+' *> self
        <|>
          higher
      , \higher -> -- "let" expression
          flip (foldr bindLets) <$> (matchLet *> surroundedBy '(' (assignment `sepBy` matchTok ',') ')') <*> expr0
        <|>
          higher
      ]

-- | parse expressions that don't associate, either because they are not operators or because they are operators
--   that contain the expressions they operate on in start and end tokens, like parentheses, and no other operator can associate with their expressions.
nonAssociativeExpr :: GenParser Char st Expr
nonAssociativeExpr =
       number
   <|> vectorListParentheses
   <|> variableish
   <|> scadString
   <|> boolean
   <|> scadUndefined

-- | parse operations that start with a variable name,
-- including variable reference, function calling, variable list indexing, and variable list splicing.
variableish :: GenParser Char st Expr
variableish = "variable" ?:
    do
        obj <- variable
        args <- option [] (
          "function application" ?: do
              args <- surroundedBy '(' (sepBy expr0 matchComma) ')'
              pure [(:$ args)]
          )
        mods <- many (
               "list indexing" ?: do
                   i <- surroundedBy '[' expr0 ']'
                   pure $ \l -> Var "index" :$ [l, i]
          *<|> "list splicing" ?: do
                   start <- matchTok '[' *> optionMaybe expr0
                   end   <- matchColon *> optionMaybe expr0 <* matchTok ']'
                   pure $ case (start, end) of
                              (Nothing, Nothing) -> id
                              (Just s,  Nothing)  -> \l -> Var "splice" :$ [l, s, LitE OUndefined]
                              (Nothing, Just e )  -> \l -> Var "splice" :$ [l, LitE $ ONum 0, e]
                              (Just s,  Just e )  -> \l -> Var "splice" :$ [l, s, e]
                 )
        pure $ foldl (\a b -> b a) obj (args <> mods)

-- | Parse parentheses, lists, vectors, and vector/list generators.
vectorListParentheses :: GenParser Char st Expr
vectorListParentheses =
         "vector/list/parentheses" ?: do
            -- eg. [ 3, a, a+1, b, a*b] - list
            --     ( 1, 2, 3) - list
            --     (a+1) - parenthesized expression.
            o <- oneOf "[(" <* whiteSpace
            exprs <- sepBy expr0 matchComma
              <* if o == '['
                 then matchTok ']'
                 else matchTok ')'
            pure $ if o == '(' && length exprs == 1
                     then head exprs
                     else ListE exprs
    *<|> "vector/list generator" ?: do
        -- eg.  [ a : 1 : a + 10 ]
        --      [ a : a + 10 ]
        -- FIXME: clearly, these have a numeric context, and should fail to parse for non-numeric contents.
        expr1 <- matchTok '[' *> expr0 <* matchColon
        exprs <- do
                   expr2 <- expr0
                   expr3 <- optionMaybe (matchColon *> expr0)
                   pure $ case expr3 of
                      Just n  -> [expr1, expr2, n]
                      Nothing -> [expr1, LitE $ ONum 1.0, expr2]
          <* matchTok ']'
        pure $ collector "list_gen" exprs

-- | Apply a symbolic operator to a list of expressions, pureing one big expression.
--   Accepts a string for the operator, to simplify callers.
collector :: String -> [Expr] -> Expr
collector _ [x] = x
collector s  l  = Var s :$ [ListE l]

-- | Apply a symbolic operator to two expressions, combining left and right operands with an binary operator
binaryOperation :: String -> Expr -> Expr -> Expr
binaryOperation symbol left right = Var symbol :$ [left, right]

-- | An assignment expression within a let's bindings list
assignment :: GenParser Char st Expr
assignment = do
    ident       <- matchIdentifier
    expression  <- matchTok '=' *> expr0
    pure $ ListE [Var ident, expression]

-- | build nested let statements when foldr'd.
bindLets :: Expr -> Expr -> Expr
bindLets (ListE [Var boundName, boundExpr]) nestedExpr = LamE [Name boundName] nestedExpr :$ [boundExpr]
bindLets _ e = e
