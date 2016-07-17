module Graphics.Implicit.ExtOpenScad.Parser.AltExpr where

-- TODO remove tracing
import Debug.Trace;
import Control.Monad.Fix
import Text.ParserCombinators.Parsec  hiding (State)
import Graphics.Implicit.ExtOpenScad.Definitions
import Graphics.Implicit.ExtOpenScad.Parser.Lexer

nonOperator = do
        matchTrue
        return $ LitE $ OBool True
    <|> do
        matchFalse
        return $ LitE $ OBool False
    <|> do
        matchUndef
        return $ LitE OUndefined
    <|> do
        n <- number
        case n of
            Left integer -> return $ LitE $ ONum $ fromIntegral integer
            Right double -> return $ LitE $ ONum double
    <|> do
        str <- literalString
        return $ LitE $ OString str
    <|> do
        ident <- identifier
        return $ Var ident
    <|> do
        _ <- matchChar '('
        _ <- matchChar ')'
        return $ LitE $ OString "()"

binaryOperation :: String -> Expr -> Expr -> Expr
binaryOperation symbol left right = Var symbol :$ [left, right]

assignment :: Parser Expr
assignment = (do
    ident <- identifier
    matchChar '='
    expr <- expr
    return $ ListE [Var ident, expr]
    )

bindLets :: Expr -> Expr -> Expr
bindLets (ListE [Var boundName, boundExpr]) nestedExpr = (LamE [Name boundName] nestedExpr) :$ [boundExpr]
bindLets _ e = e

-- Borrowed the pattern from http://compgroups.net/comp.lang.functional/parsing-ternary-operator-with-parsec/1052460
-- In the levels list, the first element is the lowest precedent, and the last is the highest.
-- "higher" represents the higher precedence parser, ie. the next one in the levels list.
-- "fix $ \self ->..." is used to consume all expressions in the same level, "self" being the current level.
expr = foldr ($) nonOperator levels
    where
        levels = 
          [ id
          , \higher -> fix $ \self -> -- "let" expression
                do
                        matchLet
                        matchChar '('
                        bindings <- sepBy assignment (matchChar ',')
                        matchChar ')'
                        expr <- self
                        return $ foldr bindLets expr bindings
                <|> higher
          , \higher -> fix $ \self -> -- ternary operator, ? :
                do  condition <- higher 
                    ( 
                        do
                            matchChar '?'
                            trueExpr <- self
                            matchChar ':'
                            falseExpr <- self
                            return $ Var "?" :$ [condition, trueExpr, falseExpr]
                        <|> return condition
                        )
          , \higher -> -- || operator
                do
                    chainl1 higher (do
                        op <- matchOR
                        return $ binaryOperation op
                        )
          , \higher -> -- && operator
                do
                    chainl1 higher (do
                        op <- matchAND
                        return $ binaryOperation op
                        )
          , \higher -> -- <, <=, >=, > comparison operators
                do
                    chainl1 higher (do
                        op <- matchChar '<' <|> matchLE <|> matchGE <|> matchChar '>'
                        return $ binaryOperation op
                        )
          , \higher -> -- == and != operators
                do
                    chainl1 higher (do
                        op <-(matchEQ <|> matchNE)
                        return $ binaryOperation op
                        )
          , \higher -> -- + and - operators
                do
                    chainl1 higher (do
                        op <- matchChar '-' <|> matchChar '+'
                        return $ binaryOperation op
                        )
          , \higher -> fix $ \self -> do -- OpenSCAD's YACC parser puts '!' at the same level of precedence as '-' and '+'. I think the semantics are the same.
                do
                    bang <- matchChar '!'
                    right <- self
                    return $ Var bang :$ [right]
                <|> higher
          , \higher -> -- *, /, % operators
                do
                    chainl1 higher (do
                        op <- matchChar '*' <|> matchChar '/' <|> matchChar '%'
                        return $ binaryOperation op
                        )
            ]
