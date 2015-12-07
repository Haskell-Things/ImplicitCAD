module Graphics.Implicit.ExtOpenScad.Definitions where

import Graphics.Implicit.Definitions
import Data.Map (Map)

type Symbol = String

data Pattern = Name  Symbol
             | ListP  [Pattern]
             | Wild
             | Symbol :@ Pattern
    deriving (Show, Eq)

data Expr = Var Symbol
          | LitE OVal
          | ListE [Expr]
          | LamE [Pattern] Expr
          | Expr :$ [Expr]
    deriving (Show, Eq)

data StatementI = StatementI Int (Statement StatementI)
    deriving (Show, Eq)

data Statement st = Include String Bool
               | Pattern :=  Expr
               | Echo [Expr]
               | For Pattern Expr [st]
               | If Expr [st] [st]
               | NewModule  Symbol [(Symbol, Maybe Expr)] [st]
               | ModuleCall Symbol [(Maybe Symbol, Expr)] [st]
               | DoNothing
    deriving (Show, Eq)



-- | Objects for our OpenSCAD-like language
data OVal = OUndefined 
         | OError [String]
         | OBool Bool 
         | ONum ℝ
         | OList [OVal]
         | OString String
         | OFunc (OVal -> OVal)
         | OModule ([OVal] -> ArgParser (IO [OVal]))
         | OObj3 SymbolicObj3
         | OObj2 SymbolicObj2

instance Eq OVal where
    (OBool a) == (OBool b) = a == b
    (ONum  a) == (ONum  b) = a == b
    (OList a) == (OList b) = all id $ zipWith (==) a b
    (OString a) == (OString b) = a == b
    _ == _ = False

instance Show OVal where
    show OUndefined = "Undefined"
    show (OBool b) = show b
    show (ONum n) = show n
    show (OList l) = show l
    show (OString s) = show s
    show (OFunc _) = "<function>"
    show (OModule _) = "module"
    show (OError msgs) = "Execution Error:\n" ++ foldl1 (\a b -> a ++ "\n" ++ b) msgs
    show (OObj2 obj) = "<obj2: " ++ show obj ++ ">"
    show (OObj3 obj) = "<obj3: " ++ show obj ++ ">"

type VarLookup = Map String OVal
type FStack = [OVal]

collector :: Symbol -> [Expr] -> Expr
collector _ [x] = x
collector s  l  = Var s :$ [ListE l]

-----------------------------------------------------------------
-- | Handles parsing arguments to modules
data ArgParser a 
                 -- | For actual argument entries:
                 --   ArgParser (argument name) (default) (doc) (next Argparser...)
                 = AP String (Maybe OVal) String (OVal -> ArgParser a) 
                 -- | For returns:
                 --   ArgParserTerminator (return value)
                 | APTerminator a 
                 -- | For failure:
                 --   ArgParserFailIf (test) (error message) (child for if true)
                 | APFailIf Bool String (ArgParser a)
                 --  An example, then next
                 | APExample String (ArgParser a)
                 --  A string to run as a test, then invariants for the results, then next
                 | APTest String [TestInvariant] (ArgParser a)
                 -- A branch where there are a number of possibilities for the parser underneath
                 | APBranch [ArgParser a]

data TestInvariant = EulerCharacteristic Int 
    deriving (Show)

