{- ORMOLU_DISABLE -}
-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright (C) 2016, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

-- Allow us to use string literals to represent Text.
{-# LANGUAGE OverloadedStrings #-}

-- Allow the use of \case
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module Graphics.Implicit.ExtOpenScad.Default (defaultObjects) where

-- be explicit about where we pull things in from.
import Prelude (Bool(True, False), Maybe(Just, Nothing), ($), (<>), (<$>), fmap, pi, sin, cos, tan, asin, acos, atan, sinh, cosh, tanh, abs, signum, fromInteger, (.), floor, ceiling, round, exp, log, sqrt, max, min, atan2, (**), flip, (<), (>), (<=), (>=), (==), (/=), (&&), (||), not, show, foldl, (*), (/), mod, (+), zipWith, (-), otherwise, id, foldMap, fromIntegral, IO, pure, Int, isNaN, negate, RealFloat)
import qualified Prelude as P (length)

import Graphics.Implicit.Definitions (ℝ, ℕ)

import Graphics.Implicit.ExtOpenScad.Definitions (VarLookup(VarLookup), OVal(OBool, OList, ONum, OString, OUndefined, OError, OFunc, OVargsModule, OIO), Symbol(Symbol), StateC, StatementI, SourcePosition, MessageType(TextOut, Warning), ScadOpts(ScadOpts))

import Graphics.Implicit.ExtOpenScad.Util.OVal (toOObj, oTypeStr)

import Graphics.Implicit.ExtOpenScad.Primitives (primitiveModules)

import Graphics.Implicit.ExtOpenScad.Util.StateC (scadOptions, modifyVarLookup, addMessage)

import Data.Int (Int64)

import Data.Map (Map, fromList, insert)

import Data.List (genericIndex, genericLength, find, foldl')

import Data.Foldable (for_, foldr)

import qualified Data.Text.Lazy as TL (index)

import Data.Text.Lazy (Text, intercalate, unpack, pack, length, singleton)
import Control.Monad (replicateM)
import System.Random (randomRIO)
import Data.Maybe (maybe)
import Data.Tuple (snd)
import Linear.Matrix ((!*!), (*!), (!*))
import Graphics.Implicit.MathUtil (infty)
import Data.Ord (clamp)

defaultObjects :: Bool -> VarLookup
defaultObjects withCSG = VarLookup $ fromList $
    defaultConstants
    <> defaultFunctions
    <> defaultFunctions2
    <> defaultFunctionsSpecial
    <> defaultPolymorphicFunctions
    <> (if withCSG then primitiveModules else [])
    <> varArgModules

defaultConstants :: [(Symbol, OVal)]
defaultConstants = (\(a,b) -> (a, toOObj (b :: ℝ))) <$>
    [(Symbol "pi", pi),
     (Symbol "PI", pi)]

-- Values and functions for dealing with NaNs and Infinities.
minimumValue :: ℝ
minimumValue = -1e100
maximumValue :: ℝ
maximumValue = 1e100
nanNegInf :: RealFloat a => a -> a
nanNegInf x = if isNaN x then -infty else x
signedNaNInf :: RealFloat a => a -> a -> a
signedNaNInf x y = if isNaN y then signum x * infty else y

defaultFunctions :: [(Symbol, OVal)]
defaultFunctions = (\(a,b) -> (a, toOObj ( b :: ℝ -> ℝ))) <$>
    [
        (Symbol "sin",   sin),
        (Symbol "cos",   cos),
        (Symbol "tan",   tan),
        -- If the value is NaN, set it to the signed infinity of the input
        -- and then clamp the values so that infinity doesn't propagate.
        (Symbol "asin",  \x -> clamp (minimumValue, maximumValue) . signedNaNInf x $ asin x),
        -- same as asin, but we need to invert the input sign when clamping
        (Symbol "acos",  \x -> clamp (minimumValue, maximumValue) . signedNaNInf (negate x) $ acos x),
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
        -- Log is undefined for negative values, so we are taking those NaNs
        -- and -Infinity values and clamping them to a very negative, but
        -- finite, value.
        (Symbol "ln",    clamp (minimumValue, infty) . nanNegInf . log),
        (Symbol "log",   clamp (minimumValue, infty) . nanNegInf . log),
        (Symbol "sign",  signum),
        -- same as log, but clamping to 0 rather than a very large negative value
        (Symbol "sqrt",  clamp (0, infty) . nanNegInf . sqrt)
    ]

defaultFunctions2 :: [(Symbol, OVal)]
defaultFunctions2 = (\(a,b) -> (a, toOObj (b :: ℝ -> ℝ -> ℝ))) <$>
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
            (fmap :: (OVal -> OVal) -> [OVal] -> [OVal] )
        )
    ]

varArgModules :: [(Symbol, OVal)]
varArgModules =
    [
        modVal "echo" echo
       ,modVal "for" for
       ,modVal "color" executeSuite
    ] where
        modVal name func = (Symbol name, OVargsModule (Symbol name) func)

        -- execute only the child statement, without doing anything else. Useful for unimplemented functions.
        executeSuite :: Symbol -> SourcePosition -> [(Maybe Symbol, OVal)] -> [StatementI] -> ([StatementI] -> StateC ()) -> StateC ()
        executeSuite (Symbol name) pos _ suite runSuite = do
            addMessage Warning pos $ "Module " <> name <> " not implemented"
            runSuite suite

        echo :: Symbol -> SourcePosition -> [(Maybe Symbol, OVal)] -> [StatementI] -> ([StatementI] -> StateC ()) -> StateC ()
        echo _ pos args suite runSuite = do
            scadOpts <- scadOptions
            let
                text :: [(Maybe Symbol, OVal)] -> Text
                text a = intercalate ", " $ show' <$> a
                show' :: (Maybe Symbol, OVal) -> Text
                show' (Nothing, arg) = pack $ show arg
                show' (Just (Symbol var), arg) = var <> " = " <> pack (show arg)
                showe' :: (Maybe Symbol, OVal) -> Text
                showe' (Nothing, OString arg) = arg
                showe' (Just (Symbol var), arg) = var <> " = " <> showe' (Nothing, arg)
                showe' a = show' a
                compat (ScadOpts compat_flag _) = compat_flag
                openScadFormat = "ECHO: " <> text args
                extopenscadFormat = foldMap showe' args
                formattedMessage = if compat scadOpts then openScadFormat else extopenscadFormat
            addMessage TextOut pos formattedMessage
            runSuite suite

        for :: Symbol -> SourcePosition -> [(Maybe Symbol, OVal)] -> [StatementI] -> ([StatementI] -> StateC ()) -> StateC ()
        for _ _ args suite runSuite =
            for_ (iterator args) $ \iter -> do
                modifyVarLookup iter
                runSuite suite
          where
            -- convert a list of arguments into a list of functions to transform the VarLookup with new bindings for each possible iteration.
            iterator :: [(Maybe Symbol, OVal)] -> [VarLookup -> VarLookup]
            iterator [] = [id]
            iterator ((Nothing, _):iterators) = iterator iterators
            iterator ((Just var, vals):iterators) = [outer . varify inner | inner <- insert var <$> valsList vals, outer <- iterator iterators]
            -- convert the loop iterator variable's expression value to a list (possibly of one value)
            valsList :: OVal -> [OVal]
            valsList v@(OBool _) = [v]
            valsList v@(ONum _) = [v]
            valsList v@(OString _) = [v]
            valsList (OList vs) = vs
            valsList _ = []
            -- promote a result into a VarLookup
            varify :: (Map Symbol OVal -> Map Symbol OVal) -> VarLookup -> VarLookup
            varify f (VarLookup v) = VarLookup $ f v

-- | more complicated ones:
defaultPolymorphicFunctions :: [(Symbol, OVal)]
defaultPolymorphicFunctions =
    [
        (Symbol "+", toOObj add),
        (Symbol "sum", sumtotal),
        (Symbol "*", toOObj mult),
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
        (Symbol "<>", concatenate),
        (Symbol "len", toOObj olength),
        (Symbol "str", toOObj (pack.show :: OVal -> Text)),
        (Symbol "rands", toOObj rands),
        (Symbol "lookup", toOObj lookup)
    ] where

        -- Some key functions are written as OVals in optimizations attempts

        -- Lookup a value from the given table, or linearly interpolate a value from
        -- the nearest entries. Lookups for keys that fall outside the bounds of the
        -- table will be given the value of the nearest table entry.
        -- TODO, a binary tree would be faster for large tables, but I'm not bothering
        -- until we have a good reason to do so, i.e. we see a need for it.
        lookup :: ℝ -> [(ℝ, ℝ)] -> OVal
        lookup key table =
            let
                -- Find the next lower value, and the next upper value from key
                search op op' = foldr
                    (\t@(k, _) -> maybe
                        ( if k `op` key
                          then pure t
                          else Nothing
                        )
                        $ \t'@(k', _) -> pure $
                            if k `op'` k' && k `op` key
                            then t
                            else t'
                    )
                    Nothing
                    table
                lower = search (<) (>)
                upper = search (>) (<)
                -- Interpolate linearly
                -- Take the extremes if the key is out of bounds.
                -- Undefined for empty tables, as the docs don't say what it should be.
                -- https://en.wikibooks.org/wiki/OpenSCAD_User_Manual/Mathematical_Functions#lookup
                interpolated = case (lower, upper) of
                    (Just (lk, lv), Just (uk, uv)) ->
                        -- calculate the linear slope of the graph
                        let scale = (uv - lv) / (uk - lk)
                        -- Use the lower value as the base, and add on the
                        -- required amount of scaling
                        in ONum $ lv + ((key - lk) * scale)
                    (Nothing, Just (_, uv)) -> ONum uv
                    (Just (_, lv), Nothing) -> ONum lv
                    (Nothing, Nothing)      -> OUndefined
            in maybe
                interpolated
                (ONum . snd)
                $ find (\(k, _) -> k == key) table

        rands :: ℝ -> ℝ -> ℝ -> IO OVal
        rands minR maxR count = do
            l <- replicateM (round count) $ randomRIO (minR, maxR)
            pure . OList $ ONum <$> l

        prod = OFunc $ \case
            (OList (y:ys)) -> foldl mult y ys
            (OList [])     -> ONum 1
            (ONum a)       -> OFunc $ \case
                (OList []) -> ONum a
                (OList n)  -> mult (ONum a) (OList n)
                (ONum b)   -> mult (ONum a) (ONum b)
                _          -> OError "prod takes only lists or nums"
            _              -> OError "prod takes only lists or nums"

        toNumList :: [OVal] -> Maybe [ℝ]
        toNumList [] = pure []
        toNumList (ONum r:l) = (r :) <$> toNumList l
        toNumList _ = Nothing

        -- Given a matrix, ensure that each row is
        -- at least as big as the first row, and
        -- return the dimentions.
        normaliseMatrix :: [[OVal]] -> Maybe ([[ℝ]], Int, Int) -- Matrix, outer length, inner length
        normaliseMatrix [] = Just ([], 0, 0)
        normaliseMatrix [a] = (\a' -> (pure a', 1, P.length a)) <$> toNumList a
        -- foldl is used because we need to track the length of the first sub-list throughout
        normaliseMatrix (a:as) = foldl' go base as
            where
                base :: Maybe ([[ℝ]], Int, Int)
                base = (\a' -> ([a'], 1, P.length a)) <$> toNumList a
                go:: Maybe ([[ℝ]], Int, Int) -> [OVal] -> Maybe ([[ℝ]], Int, Int)
                go Nothing _ = Nothing
                go x [] = x
                go (Just (xs, l, l')) y =
                    if P.length y >= l'
                    then (\y' -> (xs <> pure y', l + 1, l')) <$> toNumList y
                    else Nothing

        -- scalar
        mult (ONum a)  (ONum b)  = ONum  (a*b)
        -- vector-number
        mult (ONum a)  (OList b) = OList (fmap (mult (ONum a)) b)
        mult b@(OList _)  a@(ONum _) = mult a b
        -- (vector|matrix)-(vector|matrix)
        mult (OList a) (OList b) = case (aList, bList) of
            -- matrix multiplication
            (Just a', Just b') -> case (normaliseMatrix a', normaliseMatrix b') of
                (Just (as, _aOuter, aInner), Just (bs, bOuter, _bInner)) ->
                    if aInner == bOuter
                    then OList . fmap (OList . fmap ONum) $ as !*! bs
                    else OError "Matrices of * do not have a matching M dimention for NxM and MxP"
                (Nothing, _) -> OError "First matrix of * has rows that are too short."
                (_, Nothing) -> OError "Second matrix of * has rows that are too short."
            -- matrix * vector multiplication
            -- These aren't commutative so we have to do it the hard way
            -- https://en.wikibooks.org/wiki/OpenSCAD_User_Manual/Mathematical_Operators
            (Just a', _) -> case normaliseMatrix a' of
                Just (as, _aOuter, aInner) ->
                    if P.length b >= aInner
                    then
                        maybe
                            (OError "Second vector of * is not a list of numbers.")
                            (\b' -> OList . fmap ONum $ as !* b')
                            $ toNumList b
                    else OError "Second vector of * is too short to multiply with the matrix."
                _ -> OError "First matrix of * has rows that are too short."
            -- vector * matrix multiplication
            (_, Just b') -> case normaliseMatrix b' of
                Just (bs, bOuter, _bInner) ->
                    if P.length a >= bOuter
                    then
                        maybe
                            (OError "First vector of * is not a list of numbers.")
                            (\a' -> OList . fmap ONum $ a' *! bs)
                            $ toNumList a
                    else OError "First vector of * is too short to multiply with the matrix."
                _ -> OError "Second matrix of * has rows that are too short."
            -- vector dot product
            _ -> dot
            where
                aList = foldr f (pure []) a
                bList = foldr f (pure []) b
                f :: OVal -> Maybe [[OVal]] -> Maybe [[OVal]]
                f (OList x) (Just l) = pure $ x : l
                f _ _ = Nothing
                dot = OList $ zipWith mult a b
        mult a         b         = errorAsAppropriate "product" a b

        divide = OFunc $ \case
            (ONum a) -> OFunc $ \case
                (ONum b) -> ONum (clamp (minimumValue, maximumValue) $ a/b)
                b        -> errorAsAppropriate "divide" (ONum a) b
            a -> OFunc $ \case
                b -> div' a b

        div' (ONum a)  (ONum b) = ONum  (clamp (minimumValue, maximumValue) $  a/b)
        div' (OList a) (ONum b) = OList (fmap (\x -> div' x (ONum b)) a)
        div' a         b        = errorAsAppropriate "divide" a b

        omod (ONum a) (ONum b) = ONum . fromInteger $ mod (floor a) (floor b)
        omod a        b        = errorAsAppropriate "mod" a b

        concatenate = OFunc $ \case
            (OList (y:ys)) -> foldl append y ys
            (OList [])     -> OList []
            _              -> OError "concat takes a list"

        append (OList   a) (OList   b) = OList   $ a<>b
        append (OString a) (OString b) = OString $ a<>b
        append a           b           = errorAsAppropriate "concat" a b

        sumtotal = OFunc $ \case
            (OList (y:ys)) -> foldl add y ys
            (OList [])     -> ONum 0
            (ONum a)       -> OFunc $ \case
                (OList []) -> ONum a
                (OList n)  -> add (ONum a) (OList n)
                (ONum b)   -> add (ONum a) (ONum b)
                _          -> OError "sum takes two lists or nums"
            _              -> OError "sum takes two lists or nums"

        add (ONum a) (ONum b) = ONum (a+b)
        add (ONum a)  (OList b) = OList (fmap (add (ONum a)) b)
        add (OList a) (ONum b)  = OList (fmap (add (ONum b)) a)
        add (OList a) (OList b) = OList $ zipWith add a b
        add a b = errorAsAppropriate "add" a b

        sub (ONum a) (ONum b) = ONum (a-b)
        sub (OList a) (OList b) = OList $ zipWith sub a b
        sub a b = errorAsAppropriate "subtract" a b

        negatefun (ONum n) = ONum (-n)
        negatefun (OList l) = OList $ negatefun <$> l
        negatefun a = OError $ "Can't negate " <> oTypeStr a <> "(" <> pack (show a) <> ")"

        index (OList l) (ONum ind) =
            let
                n :: ℕ
                n = floor ind
            in
              if n < genericLength l then l `genericIndex` n else OError "List accessed out of bounds"
        index (OString s) (ONum ind) =
            let
                n :: Int64
                n = floor ind
            in if n < length s then OString (singleton (TL.index s n)) else OError "List accessed out of bounds"
        -- For IO actions, get the OVal inside the IO and try to index that, rewrapping the results.
        index (OIO o) ind = OIO $ flip index ind <$> o

        index a b = errorAsAppropriate "index" a b

        osplice (OList  list) (ONum a) (    ONum b    ) =
            OList   $ splice list (floor a) (floor b)
        osplice (OString str) (ONum a) (    ONum b    ) =
            OString . pack $ splice (unpack str)  (floor a) (floor b)
        osplice (OList  list)  OUndefined  (ONum b    ) =
            OList   $ splice list 0 (floor b)
        osplice (OString str)  OUndefined  (ONum b    ) =
            OString . pack $ splice (unpack str)  0 (floor b)
        osplice (OList  list) (ONum a)      OUndefined  =
            OList   $ splice list (floor a) (genericLength list + 1)
        osplice (OString str) (ONum a)      OUndefined  =
            OString . pack $ splice (unpack str)  (floor a) (fromIntegral $ length str  + 1)
        osplice (OList  list)  OUndefined   OUndefined  =
            OList   $ splice list 0 (genericLength list + 1)
        osplice (OString str)  OUndefined   OUndefined =
            OString . pack $ splice (unpack str)  0 (fromIntegral $ length str  + 1)
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
        errorAsAppropriate name a b = OError $
          "Can't " <> name <> " objects of types " <> oTypeStr a <> " and " <> oTypeStr b <> "."

        list_gen :: [ℝ] -> Maybe [ℝ]
        list_gen [a, b] = Just $ fromInteger <$> [(ceiling a).. (floor b)]
        list_gen [a, b, c] =
            let
                nr = (c-a)/b
                n :: ℝ
                n  = fromInteger (floor nr)
            in if nr - n > 0
            then Just $ fromInteger <$> [(ceiling a), (ceiling (a+b)).. (floor (c - b*(nr -n)))]
            else Just $ fromInteger <$> [(ceiling a), (ceiling (a+b)).. (floor c)]
        list_gen _ = Nothing

        ternary :: Bool -> t -> t -> t
        ternary True a _ = a
        ternary False _ b = b

        olength (OString s) = ONum $ fromIntegral $ length s
        olength (OList s)   = ONum $ genericLength s
        olength a           = OError $ "Can't take length of a " <> oTypeStr a <> "."
