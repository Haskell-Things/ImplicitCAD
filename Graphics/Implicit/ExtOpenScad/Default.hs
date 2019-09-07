-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright (C) 2016, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

-- Allow us to use explicit foralls when writing function type declarations.
{-# LANGUAGE ExplicitForAll #-}

-- We'd like to parse openscad-ish code, with some improvements, for backwards compatability.

module Graphics.Implicit.ExtOpenScad.Default (defaultObjects) where

-- be explicit about where we pull things in from.
import Prelude (String, Bool(True, False), Maybe(Just, Nothing), ($), (++), map, pi, sin, cos, tan, asin, acos, atan, sinh, cosh, tanh, abs, signum, fromInteger, (.), floor, ceiling, round, exp, log, sqrt, max, min, atan2, (**), flip, (<), (>), (<=), (>=), (==), (/=), (&&), (||), not, show, foldl, (*), (/), mod, (+), zipWith, (-), otherwise, id, fst, snd)

import Graphics.Implicit.Definitions (ℝ, ℕ)
import Graphics.Implicit.ExtOpenScad.Definitions (VarLookup(VarLookup), OVal(OBool, OList, ONum, OString, OUndefined, OError, OModule, OFunc, OVargsModule), Symbol(Symbol), StateC, StatementI, SourcePosition, MessageType(TextOut, Warning), ScadOpts(ScadOpts))
import Graphics.Implicit.ExtOpenScad.Util.OVal (toOObj, oTypeStr)
import Graphics.Implicit.ExtOpenScad.Primitives (primitives)
import Graphics.Implicit.ExtOpenScad.Util.StateC (scadOptions, modifyVarLookup, addMessage)
import Data.Map (Map, fromList, insert)
import Data.List (genericIndex, genericLength, intercalate, concatMap)
import Control.Monad.State (forM_)

defaultObjects :: VarLookup
defaultObjects = VarLookup $ fromList $
    defaultConstants
    ++ defaultFunctions
    ++ defaultFunctions2
    ++ defaultFunctionsSpecial
    ++ defaultModules
    ++ varArgModules
    ++ defaultPolymorphicFunctions

-- Missing standard ones:
-- rand, lookup,

defaultConstants :: [(Symbol, OVal)]
defaultConstants = map (\(a,b) -> (a, toOObj (b :: ℝ) ))
    [(Symbol "pi", pi),
     (Symbol "PI", pi)]

defaultFunctions :: [(Symbol, OVal)]
defaultFunctions = map (\(a,b) -> (a, toOObj ( b :: ℝ -> ℝ)))
    [
        (Symbol "sin",   sin),
        (Symbol "cos",   cos),
        (Symbol "tan",   tan),
        (Symbol "asin",  asin),
        (Symbol "acos",  acos),
        (Symbol "atan",  atan),
        (Symbol "sinh",  sinh),
        (Symbol "cosh",  cosh),
        (Symbol "tanh",  tanh),
        (Symbol "abs",   abs),
        (Symbol "sign",  signum),
        (Symbol "floor", fromInteger . floor ),
        (Symbol "ceil",  fromInteger . ceiling ),
        (Symbol "round", fromInteger . round ),
        (Symbol "exp",   exp),
        (Symbol "ln",    log),
        (Symbol "log",   log),
        (Symbol "sign",  signum),
        (Symbol "sqrt",  sqrt)
    ]

defaultFunctions2 :: [(Symbol, OVal)]
defaultFunctions2 = map (\(a,b) -> (a, toOObj (b :: ℝ -> ℝ -> ℝ) ))
    [
        (Symbol "max",   max),
        (Symbol "min",   min),
        (Symbol "atan2", atan2),
        (Symbol "pow",   (**))
    ]

defaultFunctionsSpecial :: [(Symbol, OVal)]
defaultFunctionsSpecial =
    [
        (Symbol "map", toOObj $ flip
            (map :: (OVal -> OVal) -> [OVal] -> [OVal] )
        )
    ]

defaultModules :: [(Symbol, OVal)]
defaultModules =
  map makeModule primitives
  where
    makeModule a = (fst a, OModule (fst a) Nothing (snd a))

varArgModules :: [(Symbol, OVal)]
varArgModules =
    [
        modVal "echo" echo
       ,modVal "for" for
       ,modVal "color" executeSuite
    ] where
        modVal name func = ((Symbol name), OVargsModule name func)

        -- execute only the child statement, without doing anything else. Useful for unimplemented functions.
        executeSuite :: String -> SourcePosition -> [(Maybe Symbol, OVal)] -> [StatementI] -> ([StatementI] -> StateC ()) -> StateC ()
        executeSuite name pos _ suite runSuite = do
            addMessage Warning pos $ "Module " ++ name ++ " not implemented"
            runSuite suite

        echo :: String -> SourcePosition -> [(Maybe Symbol, OVal)] -> [StatementI] -> ([StatementI] -> StateC ()) -> StateC ()
        echo _ pos args suite runSuite = do
            scadOpts <- scadOptions
            let
                text :: [(Maybe Symbol, OVal)] -> String
                text a = intercalate ", " $ map show' a
                show' :: (Maybe Symbol, OVal) -> String
                show' (Nothing, arg) = show arg
                show' (Just (Symbol var), arg) = var ++ " = " ++ show arg
                showe' (Nothing, OString arg) = arg
                showe' (Just (Symbol var), arg) = var ++ " = " ++ showe' (Nothing, arg)
                showe' a = show' a
                compat (ScadOpts compat_flag _ _) = compat_flag
                openScadFormat = "ECHO: " ++ text args
                extopenscadFormat = concatMap showe' args
                formattedMessage = if (compat scadOpts) then openScadFormat else extopenscadFormat
            addMessage TextOut pos $ formattedMessage
            runSuite suite

        for :: String -> SourcePosition -> [(Maybe Symbol, OVal)] -> [StatementI] -> ([StatementI] -> StateC ()) -> StateC ()
        for _ _ args suite runSuite =
            forM_ (iterator args) $ \iter -> do
                modifyVarLookup iter
                runSuite suite

        -- convert the loop iterator variable's expression value to a list (possibly of one value)
        valsList :: OVal -> [OVal]
        valsList v@(OBool _) = [v]
        valsList v@(ONum _) = [v]
        valsList v@(OString _) = [v]
        valsList (OList vs) = vs
        valsList _ = []

        -- convert a list of arguments into a list of functions to transform the VarLookup with new bindings for each possible iteration.
        iterator :: [(Maybe Symbol, OVal)] -> [VarLookup -> VarLookup]
        iterator [] = [id]
        iterator ((Nothing, _):iterators) = [outer | outer <- iterator iterators]
        iterator ((Just var, vals):iterators) = [outer . (varify inner) | inner <- map (insert var) (valsList vals), outer <- iterator iterators]

        varify :: (Map Symbol OVal -> Map Symbol OVal) -> VarLookup -> VarLookup
        varify f (VarLookup v) = VarLookup $ f v

-- more complicated ones:

defaultPolymorphicFunctions :: [(Symbol, OVal)]
defaultPolymorphicFunctions =
    [
        (Symbol "+", sumtotal),
        (Symbol "sum", sumtotal),
        (Symbol "*", prod),
        (Symbol "prod", prod),
        (Symbol "/", divide),
        (Symbol "-", toOObj sub),
        (Symbol "%", toOObj omod),
        (Symbol "^", toOObj ((**) :: ℝ -> ℝ -> ℝ)),
        (Symbol "negate", toOObj negatefun),
        (Symbol "index", toOObj index),
        (Symbol "splice", toOObj osplice),
        (Symbol "<", toOObj  ((<) :: ℝ -> ℝ -> Bool) ),
        (Symbol ">", toOObj  ((>) :: ℝ -> ℝ -> Bool) ),
        (Symbol ">=", toOObj ((>=) :: ℝ -> ℝ -> Bool) ),
        (Symbol "<=", toOObj ((<=) :: ℝ -> ℝ -> Bool) ),
        (Symbol "==", toOObj ((==) :: OVal -> OVal -> Bool) ),
        (Symbol "!=", toOObj ((/=) :: OVal -> OVal -> Bool) ),
        (Symbol "?", toOObj ( ternary :: Bool -> OVal -> OVal -> OVal) ),
        (Symbol "&&", toOObj (&&) ),
        (Symbol "||", toOObj (||) ),
        (Symbol "!", toOObj not ),
        (Symbol "list_gen", toOObj list_gen),
        (Symbol "++", concatenate),
        (Symbol "len", toOObj olength),
        (Symbol "str", toOObj (show :: OVal -> String))
    ] where

        -- Some key functions are written as OVals in optimizations attempts

        prod = OFunc $ \x -> case x of
            (OList (y:ys)) -> foldl mult y ys
            (OList [])     -> ONum 1
            (ONum a)       -> OFunc $ \y -> case y of
                (OList []) -> ONum a
                (OList n)  -> mult (ONum a) (OList n)
                (ONum b)   -> mult (ONum a) (ONum b)
                _          -> OError ["prod takes two lists or nums"]
            _              -> OError ["prod takes two lists or nums"]

        mult (ONum a)  (ONum b)  = ONum  (a*b)
        mult (ONum a)  (OList b) = OList (map (mult (ONum a)) b)
        mult (OList a) (ONum b)  = OList (map (mult (ONum b)) a)
        mult a         b         = errorAsAppropriate "product" a b

        divide = OFunc $ \x -> case x of
            (ONum a) -> OFunc $ \y -> case y of
                (ONum b) -> ONum (a/b)
                b        -> errorAsAppropriate "divide" (ONum a) b
            a -> OFunc $ \y -> case y of
                b -> div' a b

        div' (ONum a)  (ONum b) = ONum  (a/b)
        div' (OList a) (ONum b) = OList (map (\x -> div' x (ONum b)) a)
        div' a         b        = errorAsAppropriate "divide" a b

        omod (ONum a) (ONum b) = ONum . fromInteger $ mod (floor a) (floor b)
        omod a        b        = errorAsAppropriate "mod" a b

        concatenate = OFunc $ \x -> case x of
            (OList (y:ys)) -> foldl append y ys
            (OList [])     -> OList []
            _              -> OError ["concat takes a list"]

        append (OList   a) (OList   b) = OList   $ a++b
        append (OString a) (OString b) = OString $ a++b
        append a           b           = errorAsAppropriate "concat" a b

        sumtotal = OFunc $ \x -> case x of
            (OList (y:ys)) -> foldl add y ys
            (OList [])     -> ONum 0
            _              -> OError ["Sum takes a list"]

        add (ONum a) (ONum b) = ONum (a+b)
        add (OList a) (OList b) = OList $ zipWith add a b
        add a b = errorAsAppropriate "add" a b

        sub (ONum a) (ONum b) = ONum (a-b)
        sub (OList a) (OList b) = OList $ zipWith sub a b
        sub a b = errorAsAppropriate "subtract" a b

        negatefun (ONum n) = ONum (-n)
        negatefun (OList l) = OList $ map negatefun l
        negatefun a = OError ["Can't negate " ++ oTypeStr a ++ "(" ++ show a ++ ")"]

        {-numCompareToExprCompare :: (ℝ -> ℝ -> Bool) -> Oval -> OVal -> Bool
        numCompareToExprCompare f a b =
            case (fromOObj a :: Maybe ℝ, fromOObj b :: Maybe ℝ) of
                (Just a, Just b) -> f a b
                _ -> False-}

        index (OList l) (ONum ind) =
            let
                n :: ℕ
                n = floor ind
            in
              if n < genericLength l then l `genericIndex` n else OError ["List accessd out of bounds"]
        index (OString s) (ONum ind) =
            let
                n :: ℕ
                n = floor ind
            in if n < genericLength s then OString [s `genericIndex` n] else OError ["List accessd out of bounds"]
        index a b = errorAsAppropriate "index" a b

        osplice (OList  list) (ONum a) (    ONum b    ) =
            OList   $ splice list (floor a) (floor b)
        osplice (OString str) (ONum a) (    ONum b    ) =
            OString $ splice str  (floor a) (floor b)
        osplice (OList  list)  OUndefined  (ONum b    ) =
            OList   $ splice list 0 (floor b)
        osplice (OString str)  OUndefined  (ONum b    ) =
            OString $ splice str  0 (floor b)
        osplice (OList  list) (ONum a)      OUndefined  =
            OList   $ splice list (floor a) (genericLength list + 1)
        osplice (OString str) (ONum a)      OUndefined  =
            OString $ splice str  (floor a) (genericLength str  + 1)
        osplice (OList  list)  OUndefined   OUndefined  =
            OList   $ splice list 0 (genericLength list + 1)
        osplice (OString str)  OUndefined   OUndefined =
            OString $ splice str  0 (genericLength str  + 1)
        osplice _ _ _ = OUndefined

        splice :: [a] -> ℕ -> ℕ -> [a]
        splice [] _ _     = []
        splice l@(x:xs) a b
            |    a < 0  =    splice l   (a+n)  b
            |    b < 0  =    splice l    a    (b+n)
            |    a > 0  =    splice xs  (a-1) (b-1)
            |    b > 0  = x: splice xs   a    (b-1)
            | otherwise = []
                    where
                      n :: ℕ
                      n = genericLength l

        errorAsAppropriate _   err@(OError _)   _ = err
        errorAsAppropriate _   _   err@(OError _) = err
        errorAsAppropriate name a b = OError
            ["Can't " ++ name ++ " objects of types " ++ oTypeStr a ++ " and " ++ oTypeStr b ++ "."]

        list_gen :: [ℝ] -> Maybe [ℝ]
        list_gen [a, b] = Just $ map fromInteger [(ceiling a).. (floor b)]
        list_gen [a, b, c] =
            let
                nr = (c-a)/b
                n :: ℝ
                n  = fromInteger (floor nr)
            in if nr - n > 0
            then Just $ map fromInteger
                [(ceiling a), (ceiling (a+b)).. (floor (c - b*(nr -n)))]
            else Just $ map fromInteger
                [(ceiling a), (ceiling (a+b)).. (floor c)]
        list_gen _ = Nothing

        ternary :: forall t. Bool -> t -> t -> t
        ternary True a _ = a
        ternary False _ b = b

        olength (OString s) = ONum $ genericLength s
        olength (OList s)   = ONum $ genericLength s
        olength a           = OError ["Can't take length of a " ++ oTypeStr a ++ "."]
