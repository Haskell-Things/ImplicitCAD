module Graphics.Implicit.ExtOpenScad.Definitions where

import Graphics.Implicit.Definitions
import Data.Map (Map)
import qualified Control.Monad as Monad
import           Control.Monad.State (State,StateT)

type Symbol = String

data Pattern = Name  Symbol
             | ListP  [Pattern]
             | Wild
             | Symbol :@ Pattern
	deriving Show

data Expr = Var Symbol
          | LitE OVal
          | ListE [Expr]
          | LamE [Pattern] Expr
          | Expr :$ [Expr]
	deriving Show

data StatementI = StatementI Int (Statement StatementI)
	deriving Show

data Statement st = Include String Bool
               | Pattern :=  Expr
               | Echo [Expr]
               | For Pattern Expr [st]
               | If Expr [st] [st]
               | NewModule  Symbol [(Symbol, Maybe Expr)] [st]
               | ModuleCall Symbol [(Maybe Symbol, Expr)] [st]
               | DoNothing
	deriving Show

type CompState = (VarLookup, [OVal])
type StateC = StateT CompState IO

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

instance Show OVal where
	show OUndefined = "Undefined"
	show (OBool b) = show b
	show (ONum n) = show n
	show (OList l) = show l
	show (OString s) = show s
	show (OFunc f) = "<function>"
	show (OModule _) = "module"
	show (OError msgs) = "Execution Error:\n" ++ foldl1 (\a b -> a ++ "\n" ++ b) msgs

type VarLookup = Map String OVal
type FStack = [OVal]

collector s [x] = x
collector s  l  = Var s :$ [ListE l]

-----------------------------------------------------------------
-- | Handles parsing arguments to modules
data ArgParser a 
                 -- | For actual argument entries:
                 --   ArgParser (argument name) (default) (doc) (next Argparser...)
                 = ArgParser String (Maybe OVal) String (OVal -> ArgParser a) 
                 -- | For returns:
                 --   ArgParserTerminator (return value)
                 | ArgParserTerminator a 
                 -- | For failure:
                 --   ArgParserFailIf (test) (error message) (child for if true)
                 | ArgParserFailIf Bool String (ArgParser a)
                 --  An example, then next
                 | ArgParserExample String (ArgParser a)
                 --  A string to run as a test, then invariants for the results, then next
                 | ArgParserTest String [TestInvariant] (ArgParser a)
	deriving (Show)

data TestInvariant = EulerCharacteristic Int 
	deriving (Show)

