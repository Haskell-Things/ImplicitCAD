-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright (C) 2016, Kelvin Cookshaw (kelvin@cookshaw.com)
-- Copyright 2014 2015 2016, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

-- Allow us to use a shorter form of Var.
{-# LANGUAGE PatternSynonyms #-}

-- Enable explicit-forall syntax.
{-# LANGUAGE ExplicitForAll #-}

module Graphics.Implicit.ExtOpenScad.Parser.AltExpr (expr0) where

import Prelude (Char, String, ($), return, id, foldr)

import Control.Monad.Fix(fix)

import Text.Parsec.String (GenParser)

import Text.Parsec ((<|>), sepBy, chainl1, chainr1, oneOf)

import Graphics.Implicit.ExtOpenScad.Definitions (Expr(ListE, LitE, LamE, (:$)), Symbol(Symbol), Pattern (Name), OVal(ONum))

import qualified Graphics.Implicit.ExtOpenScad.Definitions as GIED (Expr(Var))

import Graphics.Implicit.ExtOpenScad.Parser.AltLexer(matchOR, matchAND, matchLE, matchGE, matchEQ, matchNE, matchCAT)

import Graphics.Implicit.ExtOpenScad.Parser.Lexer(matchLet, matchTok, matchIdentifier, surroundedBy, whiteSpace)

import Graphics.Implicit.ExtOpenScad.Parser.Util(number, variable, boolean, scadString, scadUndefined)

import Text.Parsec.Prim (ParsecT)

import Data.Functor.Identity (Identity)

-- Let us use the old syntax when defining Vars.
pattern Var :: String -> Expr
pattern Var n = GIED.Var (Symbol n)

-- TODO use the more helpful parser combinators like option, optional, between.

expr0 :: GenParser Char st Expr
expr0 = expr

-- parse expressions that don't associate, either because they are not operators or because they are operators
-- that contain the expressions they operate on in start and end tokens, like parentheses, and no other operator can associate with their expressions.
nonAssociativeExpr :: GenParser Char st Expr
nonAssociativeExpr = do
        boolean
    <|> scadUndefined
    <|> number -- integer or double precision number
    <|> scadString
    <|> variable
    <|> -- parenthesized expression
        surroundedBy '(' expr ')'
    <|>
        matchVectorOrRange
--    <?> "an expression"

-- Borrowed the pattern from http://compgroups.net/comp.lang.functional/parsing-ternary-operator-with-parsec/1052460
-- In the levels list, the first element is the lowest precedent, and the last is the highest.
-- "higher" represents the higher precedence parser, ie. the next one in the levels list.
-- "fix $ \self ->..." is used to consume all expressions in the same level, "self" being the current level.
expr :: GenParser Char st Expr
expr = foldr ($) nonAssociativeExpr levels
    where
        levels :: [ParsecT String u Identity Expr -> ParsecT String u Identity Expr]
        levels =
          [ id

          , \higher -> fix $ \self -> do -- ?: ternary operator.
               condition <- higher
               do
                    _         <- matchTok '?'
                    trueExpr  <- self
                    _         <- matchTok ':'
                    falseExpr <- self
                    return $ Var "?" :$ [condition, trueExpr, falseExpr]
                <|>
                    return condition

          , \higher -> -- || boolean OR operator
                chainl1 higher (do
                    op <- matchOR
                    return $ binaryOperation op)

          , \higher -> -- && boolean AND operator
                chainl1 higher (do
                    op <- matchAND
                    return $ binaryOperation op)

          , \higher -> -- <, <=, >=, > comparison operators
                chainl1 higher (do
                    op <- matchTok '<' <|> matchLE <|> matchGE <|> matchTok '>'
                    return $ binaryOperation op)

          , \higher -> -- == and != operators
                chainl1 higher (do
                    op <- matchEQ <|> matchNE
                    return $ binaryOperation op)

          , \higher -> -- + and - operators
                chainl1 higher (do
                    op <- oneOf "+-"
                    _  <- whiteSpace
                    return $ binaryOperation (op:""))

          , \higher -> -- ++ string catenation operator. This is an ExtOpenScad operation that is not available in OpenSCAD.
                chainl1 higher (do
                    op <- matchCAT
                    return $ binaryOperation op)

          , \higher -> fix $ \self -> -- unary ! operator. OpenSCAD's YACC parser puts '!' at the same level of precedence as '-' and '+'. I think the semantics are the same. Requires extensive testing.
                do
                    op    <- matchTok '!'
                    right <- self
                    return $ Var op :$ [right]
                <|> higher

          , \higher -> -- ^ exponent operator. This is not available in OpenSCAD.
                chainr1 higher (do
                        op <- matchTok '^'
                        return $ binaryOperation op)

          , \higher -> -- *, /, % operators
                chainl1 higher (do
                        op <- matchTok '*' <|> matchTok '/' <|> matchTok '%'
                        return $ binaryOperation op)

          , \higher -> fix $ \self -> -- Not sure where OpenSCAD puts this in the order of operations, but C++ puts it about here.
                do -- Unary -. -- Unary - applied to strings is undefined, but handle that in the interpreter.
                    _     <- matchTok '-'
                    right <- self
                    return $ Var "negate" :$ [right]
                <|> do -- Unary +. Handle this by ignoring the +
                    _ <- matchTok '+'
                    self
                <|> higher

          , \higher ->
                do left <- higher -- function call and vector index - in OpenSCAD a function call can only happen to a identifier.
                                  -- In ExtOpenScad a function call can happen to any expression that returns a function (or lambda expression)
                   functionCallAndIndex left

          , \higher -> do -- "let" expression
                _          <- matchLet
                bindings   <- surroundedBy '(' (assignment `sepBy` matchTok ',') ')'
                expression <- expr
                return $ foldr bindLets expression bindings
            <|>
                higher
          ]

functionCallAndIndex :: Expr -> GenParser Char st Expr
functionCallAndIndex left =
    do -- function call of function returned by the expression to the left
        arguments <- surroundedBy '(' (expr `sepBy` matchTok ',') ')'
        functionCallAndIndex $ left :$ arguments
    <|> do -- vector index of vector returned by the expression to the left
        index <- surroundedBy '[' expr ']'
        functionCallAndIndex $ Var "index" :$ [left, index]
    <|> -- no match, just return the left expression
        return left

--There are several non-associative things that begin and end with [ and ]. This parser does not handle vector indexing.
matchVectorOrRange :: GenParser Char st Expr
matchVectorOrRange = do
        _ <- matchTok '['
        do
            _ <- matchTok ']'
            return $ ListE []
         <|> do
            first <- expr
            do
                _ <- matchTok ']'
                return $ ListE [first]
             <|> do
                _     <- matchTok ','
                exprs <- sepBy expr (matchTok ',')
                _     <- matchTok ']'
                return $ ListE $ first:exprs
             <|> do
                _      <- matchTok ':'
                second <- expr
                do
                    _     <- matchTok ':'
                    third <- expr
                    _     <- matchTok ']'
                    return $ Var "list_gen" :$ [ListE [first, second, third]]
                 <|> do
                    _ <- matchTok ']'
                    return $ Var "list_gen" :$ [ListE [first, LitE $ ONum 1.0, second]]

-- combine left and right operands with an binary operator
binaryOperation :: String -> Expr -> Expr -> Expr
binaryOperation symbol left right = Var symbol :$ [left, right]

-- an assignment expression within a let's bindings list
assignment :: GenParser Char st Expr
assignment = do
    ident       <- matchIdentifier
    _           <- matchTok '='
    expression  <- expr
    return $ ListE [Var ident, expression]

-- build nested let statements when foldr'd.
bindLets :: Expr -> Expr -> Expr
bindLets (ListE [Var boundName, boundExpr]) nestedExpr = LamE [Name (Symbol boundName)] nestedExpr :$ [boundExpr]
bindLets _ e = e
