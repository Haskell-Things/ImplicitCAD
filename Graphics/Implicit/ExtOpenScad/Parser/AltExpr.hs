module Graphics.Implicit.ExtOpenScad.Parser.AltExpr (expr0, altExpr) where

-- TODO remove tracing
import Debug.Trace

import Control.Monad.Fix
import Text.ParserCombinators.Parsec  hiding (State)
import Graphics.Implicit.ExtOpenScad.Definitions
import Graphics.Implicit.ExtOpenScad.Parser.Lexer

altExpr = expr0

expr0 = do
        expr
--    <?> "an expression"
    
-- parse expressions that don't associate, either because they are not operators or because they are operators 
-- that contain the expressions they operate on in start and end tokens, like parentheses, and no other operator can associate with their expressions.
nonAssociativeExpr :: GenParser Char st Expr
nonAssociativeExpr = do -- boolean true
        matchTrue
        return $ LitE $ OBool True
    <|> do -- boolean false
        matchFalse
        return $ LitE $ OBool False
    <|> do -- undef
        matchUndef
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
        matchChar '('
        expr <- expr
        matchChar ')'
        return expr
    <|> do
        matchVectorOrRange
--    <?> "an expression"

--There are several non-associative things that begin and end with [, ]. This parser does not handle vector indexing.
matchVectorOrRange :: GenParser Char st Expr
matchVectorOrRange = do
        matchChar '['
        (do
            matchChar ']'
            return $ ListE []
         <|> do
            first <- expr
            (do
                matchChar ']'
                return $ ListE [first]
             <|> do
                matchChar ','
                exprs <- sepBy expr (matchChar ',')
                matchChar ']'
                return $ ListE $ first:exprs
             <|> do
                matchChar ':'
                second <- expr
                (do
                    matchChar ':'
                    third <- expr
                    matchChar ']'
                    return $ Var "list_gen" :$ [ListE [first, second, third]]
                 <|> do
                    matchChar ']'
                    return $ Var "list_gen" :$ [ListE [first, LitE $ ONum 1.0, second]]
                    )
                )
            )

-- combine left and right operands with an binary operator
binaryOperation :: String -> Expr -> Expr -> Expr
binaryOperation symbol left right = Var symbol :$ [left, right]

-- an assignment expression within a let's bindings list
assignment :: GenParser Char st Expr
assignment = do
    ident <- identifier
    matchChar '='
    expr <- expr
    return $ ListE [Var ident, expr]

-- build nested let statements when foldr'd.
bindLets :: Expr -> Expr -> Expr
bindLets (ListE [Var boundName, boundExpr]) nestedExpr = (LamE [Name boundName] nestedExpr) :$ [boundExpr]
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
               (do
                    matchChar '?'
                    trueExpr <- self
                    matchChar ':'
                    falseExpr <- self
                    return $ Var "?" :$ [condition, trueExpr, falseExpr]
                <|>
                    return condition)

          , \higher -> do -- || boolean OR operator
                chainl1 higher (do
                    op <- matchOR
                    return $ binaryOperation op)
                    
          , \higher -> do -- && boolean AND operator
                chainl1 higher (do
                    op <- matchAND
                    return $ binaryOperation op)
                    
          , \higher -> do -- <, <=, >=, > comparison operators
                chainl1 higher (do
                    op <- matchChar '<' <|> matchLE <|> matchGE <|> matchChar '>'
                    return $ binaryOperation op)
                    
          , \higher -> do -- == and != operators
                chainl1 higher (do
                    op <-(matchEQ <|> matchNE)
                    return $ binaryOperation op)
                    
          , \higher -> do -- + and - operators
                chainl1 higher (do
                    op <- matchChar '+' <|> matchChar '-'
                    return $ binaryOperation op)

          , \higher -> do -- ++ string catenation operator. This is an ExtOpenScad operation that is not available in OpenSCAD.
                chainl1 higher (do
                    op <- matchCAT
                    return $ binaryOperation op)

          , \higher -> fix $ \self -> -- unary ! operator. OpenSCAD's YACC parser puts '!' at the same level of precedence as '-' and '+'. I think the semantics are the same. Requires extensive testing.
                do
                    op <- matchChar '!'
                    right <- self
                    return $ Var op :$ [right]
                <|> higher

          , \higher -> do -- ^ exponent operator. This is not available in OpenSCAD.
                chainr1 higher (do
                        op <- matchChar '^'
                        return $ binaryOperation op)

          , \higher -> do -- *, /, % operators
                chainl1 higher (do
                        op <- matchChar '*' <|> matchChar '/' <|> matchChar '%'
                        return $ binaryOperation op)

          , \higher -> fix $ \self -> -- Not sure where OpenSCAD puts this in the order of operations, but C++ puts it about here.
                do -- Unary -. -- Unary - applied to strings is undefined, but handle that in the interpreter.
                    matchChar '-'
                    right <- self
                    return $ Var "negate" :$ [right]
                <|> do -- Unary +. Handle this by ignoring the +
                    matchChar '+'
                    right <- self
                    return $ right
                <|> higher

          , \higher ->
                do left <- higher -- function call and vector index - in OpenSCAD a function call can only happen to a identifier. 
                                  -- In ExtOpenScad a function call can happen to any expression that returns a function (or lambda expression)
                   nestedExprs <- functionCallAndIndex left
                   return $ nestedExprs

          , \higher -> do -- "let" expression
                matchLet
                matchChar '('
                bindings <- sepBy assignment (matchChar ',')
                matchChar ')'
                expr <- expr
                return $ foldr bindLets expr bindings
            <|>
                higher

          ]

functionCallAndIndex :: Expr -> GenParser Char st Expr
functionCallAndIndex left = 
    do -- function call of function returned by the expression to the left
        matchChar '('
        arguments <- sepBy expr (matchChar ',')
        matchChar ')'
        right <- functionCallAndIndex $ left :$ arguments
        return $ right
    <|> do -- vector index of vector returned by the expression to the left
        matchChar '['
        index <- expr
        matchChar ']'
        right <- functionCallAndIndex $ Var "index" :$ [left, index]
        return $ right
    <|> do -- no match, just return the left expression
        return $ left
