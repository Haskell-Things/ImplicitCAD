-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright 2016, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

module Graphics.Implicit.ExtOpenScad.Definitions (ArgParser(AP, APTest, APBranch, APTerminator, APFailIf, APExample),
                                                  Symbol,
                                                  Pattern(Wild, Name, ListP),
                                                  Expr(LitE, Var, ListE, LamE, (:$)),
                                                  StatementI(StatementI),
                                                  Statement(DoNothing, NewFunction, NewModule, Sequence, Include, Echo, If, For, ModuleCall, (:=)),
                                                  OVal(ONum, OBool, OString, OList, OFunc, OUndefined, OModule, OError, OVargsModule, OObj2, OObj3),
                                                  VarLookup,
                                                  FStack,
                                                  CompState(CompState),
                                                  StateC,
                                                  TestInvariant(EulerCharacteristic),
                                                  collector) where

import Prelude(Eq, Show, String, Maybe, Bool(True, False), IO, FilePath, (==), show, map, ($), (++), undefined, and, zipWith, foldl1)

-- Resolution of the world, Integer type, and symbolic languages for 2D and 3D objects.
import Graphics.Implicit.Definitions (ℝ, ℕ, SymbolicObj2, SymbolicObj3)

import Control.Applicative (Applicative, Alternative((<|>), empty), pure, (<*>))
import Control.Monad (Functor, Monad, fmap, (>>=), mzero, mplus, MonadPlus, liftM, ap, return, (>=>))
import Data.Map (Map)
import Control.Monad.State (StateT)

-- for keeping track of the line and column number we are on in our extopenscad file.
import Text.ParserCombinators.Parsec (Line, Column)

-- | This is the state of a computation. It contains a hash of variables, an array of OVals, and a path.
newtype CompState = CompState (VarLookup, [OVal], FilePath)
type StateC = StateT CompState IO

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

instance Functor ArgParser where
    fmap = liftM

instance Applicative ArgParser where
    pure = APTerminator
    (<*>) = ap

instance Monad ArgParser where
    -- We need to describe how (>>=) works.
    -- Let's get the hard ones out of the way first.
    -- ArgParser actually
    (AP str fallback d f) >>= g = AP str fallback d (f >=> g)
    (APFailIf b errmsg child) >>= g = APFailIf b errmsg (child >>= g)
    -- These next to is easy, they just pass the work along to their child
    (APExample str child) >>= g = APExample str (child >>= g)
    (APTest str tests child) >>= g = APTest str tests (child >>= g)
    -- And an ArgParserTerminator happily gives away the value it contains
    (APTerminator a) >>= g = g a
    (APBranch bs) >>= g = APBranch $ map (>>= g) bs
    return = pure

instance MonadPlus ArgParser where
    mzero = APFailIf True "" undefined
    mplus (APBranch as) (APBranch bs) = APBranch ( as  ++  bs )
    mplus (APBranch as) b             = APBranch ( as  ++ [b] )
    mplus a             (APBranch bs) = APBranch ( a   :   bs )
    mplus a             b             = APBranch [ a   ,   b  ]

instance Alternative ArgParser where
        (<|>) = mplus
        empty = mzero

type Symbol = String

data Pattern = Name Symbol
             | ListP [Pattern]
             | Wild
             | Symbol :@ Pattern
    deriving (Show, Eq)

data Expr = Var Symbol
          | LitE OVal
          | ListE [Expr]
          | LamE [Pattern] Expr
          | Expr :$ [Expr]
    deriving (Show, Eq)

-- a statement, along with the line and column number it is found on.
data StatementI = StatementI Line Column (Statement StatementI)
    deriving (Show, Eq)

data Statement st = Include String Bool
               | Pattern :=  Expr
               | Echo [Expr]
               | For Pattern Expr [st]
               | If Expr [st] [st]
               | NewFunction Symbol [(Symbol, Maybe Expr)] Expr
               | NewModule  Symbol [(Symbol, Maybe Expr)] [st]
               | ModuleCall Symbol [(Maybe Symbol, Expr)] [st]
               | Sequence [st]
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
         | OVargsModule ([(Maybe Symbol, OVal)] -> StateC [OVal])
         | OObj3 SymbolicObj3
         | OObj2 SymbolicObj2

instance Eq OVal where
    (OBool a) == (OBool b) = a == b
    (ONum  a) == (ONum  b) = a == b
    (OList a) == (OList b) = and $ zipWith (==) a b
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
    show (OVargsModule _) = "varargs module"
    show (OError msgs) = "Execution Error:\n" ++ foldl1 (\a b -> a ++ "\n" ++ b) msgs
    show (OObj2 obj) = "<obj2: " ++ show obj ++ ">"
    show (OObj3 obj) = "<obj3: " ++ show obj ++ ">"

type VarLookup = Map String OVal
type FStack = [OVal]

collector :: Symbol -> [Expr] -> Expr
collector _ [x] = x
collector s  l  = Var s :$ [ListE l]

newtype TestInvariant = EulerCharacteristic ℕ
    deriving (Show)

