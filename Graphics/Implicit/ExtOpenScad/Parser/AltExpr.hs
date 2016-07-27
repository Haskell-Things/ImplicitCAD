module Graphics.Implicit.ExtOpenScad.Parser.AltExpr (expr0, altExpr) where

import Control.Monad.Fix
import Text.ParserCombinators.Parsec  hiding (State)
import Graphics.Implicit.ExtOpenScad.Definitions
import Graphics.Implicit.ExtOpenScad.Parser.Lexer

-- TODO use the more helpful parser combinators like option, optional, between.

altExpr :: GenParser Char st Expr
altExpr = expr0

expr0 :: GenParser Char st Expr
expr0 = expr

-- parse expressions that don't associate, either because they are not operators or because they are operators
-- that contain the expressions they operate on in start and end tokens, like parentheses, and no other operator can associate with their expressions.
nonAssociativeExpr :: GenParser Char st Expr
nonAssociativeExpr = do -- boolean true
        _ <- matchTrue
        return $ LitE $ OBool True
    <|> do -- boolean false
        _ <- matchFalse
        return $ LitE $ OBool False
    <|> do -- undef
        _ <- matchUndef
        return $ LitE OUndefined
    <|> do -- integer or double precision number
        n <- number
        case n of
            Left integer -> return $ LitE $ ONum $ fromIntegral integer
            Right double -> return $ LitE $ ONum double
    <|> do -- string literal
        str <- literalString
        return $ LitE $ OString str
    <|> do -- non-keyword identifier
        ident <- identifier
        return $ Var ident
    <|> do -- parenthesized expression
        _          <- matchTok '('
        expression <- expr
        _          <- matchTok ')'
        return expression
    <|>
        matchVectorOrRange
--    <?> "an expression"

--There are several non-associative things that begin and end with [, ]. This parser does not handle vector indexing.
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
    ident       <- identifier
    _           <- matchTok '='
    expression  <- expr
    return $ ListE [Var ident, expression]

-- build nested let statements when foldr'd.
bindLets :: Expr -> Expr -> Expr
bindLets (ListE [Var boundName, boundExpr]) nestedExpr = LamE [Name boundName] nestedExpr :$ [boundExpr]
bindLets _ e = e

-- Borrowed the pattern from http://compgroups.net/comp.lang.functional/parsing-ternary-operator-with-parsec/1052460
-- In the levels list, the first element is the lowest precedent, and the last is the highest.
-- "higher" represents the higher precedence parser, ie. the next one in the levels list.
-- "fix $ \self ->..." is used to consume all expressions in the same level, "self" being the current level.
expr :: GenParser Char st Expr
expr = foldr ($) nonAssociativeExpr levels
    where
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
                    op <-matchEQ <|> matchNE
                    return $ binaryOperation op)

          , \higher -> -- + and - operators
                chainl1 higher (do
                    op <- matchTok '+' <|> matchTok '-'
                    return $ binaryOperation op)

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
                _          <- matchTok '('
                bindings   <- sepBy assignment (matchTok ',')
                _          <- matchTok ')'
                expression <- expr
                return $ foldr bindLets expression bindings
            <|>
                higher
          ]

functionCallAndIndex :: Expr -> GenParser Char st Expr
functionCallAndIndex left =
    do -- function call of function returned by the expression to the left
        _         <- matchTok '('
        arguments <- sepBy expr (matchTok ',')
        _         <- matchTok ')'
        functionCallAndIndex $ left :$ arguments
    <|> do -- vector index of vector returned by the expression to the left
        _     <- matchTok '['
        index <- expr
        _     <- matchTok ']'
        functionCallAndIndex $ Var "index" :$ [left, index]
    <|> -- no match, just return the left expression
        return left
