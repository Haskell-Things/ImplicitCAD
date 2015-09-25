module Graphics.Implicit.ExtOpenScad.Parser.Statement where

import Text.ParserCombinators.Parsec hiding (State)
import Graphics.Implicit.ExtOpenScad.Definitions
import Graphics.Implicit.ExtOpenScad.Parser.Util
import Graphics.Implicit.ExtOpenScad.Parser.Expr

parseProgram :: SourceName -> [Char] -> Either ParseError [StatementI]
parseProgram name s = parse program name s where
    program = do
        sts <- many1 computation
        eof
        return sts

-- | A  in our programming openscad-like programming language.
computation :: GenParser Char st StatementI
computation = 
    do -- suite statements: no semicolon...
        _ <- genSpace
        s <- tryMany [
            ifStatementI,
            forStatementI,
            throwAway,
            userModuleDeclaration{-,
            unimplemented "mirror",
            unimplemented "multmatrix",
            unimplemented "color",
            unimplemented "render",
            unimplemented "surface",
            unimplemented "projection",
            unimplemented "import_stl"-}
            -- rotateExtrude
            ]
        _ <- genSpace
        return s
    *<|> do -- Non suite s. Semicolon needed...
        _ <- genSpace
        s <- tryMany [
            echo,
            assignment,
            include--,
            --use
            ]
        _ <- stringGS " ; "
        return s
    *<|> do
        _ <- genSpace
        s <- userModule
        _ <- genSpace
        return s

{-
-- | A suite of s!
--   What's a suite? Consider:
--
--      union() {
--         sphere(3);
--      }
--
--  The suite was in the braces ({}). Similarily, the
--  following has the same suite:
--
--      union() sphere(3);
--
--  We consider it to be a list of s which
--  are in tern StatementI s.
--  So this parses them.
-}
suite :: GenParser Char st [StatementI]
suite = (fmap return computation <|> do 
    _ <- char '{'
    _ <- genSpace
    stmts <- many (try computation)
    _ <- genSpace
    _ <- char '}'
    return stmts
    ) <?> " suite"


throwAway :: GenParser Char st StatementI
throwAway = do
    line <- lineNumber
    _ <- genSpace
    _ <- oneOf "%*"
    _ <- genSpace
    _ <- computation
    return $ StatementI line DoNothing

-- An included ! Basically, inject another openscad file here...
include :: GenParser Char st StatementI
include = (do
    line <- lineNumber
    injectVals <-  (string "include" >> return True )
               <|> (string "use"     >> return False)
    _ <- stringGS " < "
    filename <- many (noneOf "<> ")
    _ <- stringGS " > "
    return $ StatementI line $ Include filename injectVals
    ) <?> "include "

-- | An assignment  (parser)
assignment :: GenParser Char st StatementI
assignment = ("assignment " ?:) $
    do
        line <- lineNumber
        pattern <- patternMatcher
        _ <- stringGS " = "
        valExpr <- expr0
        return $ StatementI line$ pattern := valExpr
    *<|> do
        line <- lineNumber
        varSymb <- (string "function" >> space >> genSpace >> variableSymb) 
                   *<|> variableSymb
        _ <- stringGS " ( "
        argVars <- sepBy patternMatcher (stringGS " , ")
        _ <- stringGS " ) = "
        valExpr <- expr0
        return $ StatementI line $ Name varSymb := LamE argVars valExpr

-- | An echo  (parser)
echo :: GenParser Char st StatementI
echo = do
    line <- lineNumber
    _ <- stringGS " echo ( "
    exprs <- expr0 `sepBy` (stringGS " , ")
    _ <- stringGS " ) "
    return $ StatementI line $ Echo exprs

ifStatementI :: GenParser Char st StatementI
ifStatementI = 
    "if " ?: do
        line <- lineNumber
        _ <- stringGS "if ( "
        bexpr <- expr0
        _ <- stringGS " ) "
        sTrueCase <- suite
        _ <- genSpace
        sFalseCase <- (stringGS "else " >> suite ) *<|> (return [])
        return $ StatementI line $ If bexpr sTrueCase sFalseCase

forStatementI :: GenParser Char st StatementI
forStatementI =
    "for " ?: do
        line <- lineNumber
        -- a for loop is of the form:
        --      for ( vsymb = vexpr   ) loops
        -- eg.  for ( a     = [1,2,3] ) {echo(a);   echo "lol";}
        -- eg.  for ( [a,b] = [[1,2]] ) {echo(a+b); echo "lol";}
        _ <- stringGS " for ( "
        pattern <- patternMatcher
        _ <- stringGS " = "
        vexpr <- expr0
        _ <- stringGS " ) "
        loopContent <- suite
        return $ StatementI line $ For pattern vexpr loopContent


userModule :: GenParser Char st StatementI
userModule = do
    line <- lineNumber
    name <- variableSymb
    _ <- genSpace
    args <- moduleArgsUnit
    _ <- genSpace
    s <- suite *<|> (stringGS " ; " >> return [])
    return $ StatementI line $ ModuleCall name args s

userModuleDeclaration :: GenParser Char st StatementI
userModuleDeclaration = do
    line <- lineNumber
    _ <- stringGS "module "
    newModuleName <- variableSymb
    _ <- genSpace
    args <- moduleArgsUnitDecl
    _ <- genSpace
    s <- suite
    return $ StatementI line $ NewModule newModuleName args s

----------------------

moduleArgsUnit :: GenParser Char st [(Maybe String, Expr)]
moduleArgsUnit = do
    _ <- stringGS " ( "
    args <- sepBy ( 
        do
            -- eg. a = 12
            symb <- variableSymb
            _ <- stringGS " = "
            expr <- expr0
            return $ (Just symb, expr)
        *<|> do
            -- eg. a(x,y) = 12
            symb <- variableSymb
            _ <- stringGS " ( "
            argVars <- sepBy variableSymb (try $ stringGS " , ")
            _ <- stringGS " ) = "
            expr <- expr0
            return $ (Just symb, LamE (map Name argVars) expr)
        *<|> do
            -- eg. 12
            expr <- expr0
            return (Nothing, expr)
        ) (try $ stringGS " , ")
    _ <- stringGS " ) "
    return args

moduleArgsUnitDecl ::  GenParser Char st [(String, Maybe Expr)]
moduleArgsUnitDecl = do
    _ <- stringGS " ( "
    argTemplate <- sepBy (
        do
            symb <- variableSymb;
            _ <- stringGS " = "
            expr <- expr0
            return (symb, Just expr)
        *<|> do
            symb <- variableSymb;
            _ <- stringGS " ( "
            argVars <- sepBy variableSymb (try $ stringGS " , ")
            _ <- stringGS " ) = "
            expr <- expr0
            return (symb, Just expr)
        *<|> do
            symb <- variableSymb
            return (symb, Nothing)
        ) (try $ stringGS " , ")
    _ <- stringGS " ) "
    return argTemplate

lineNumber = fmap sourceLine getPosition

