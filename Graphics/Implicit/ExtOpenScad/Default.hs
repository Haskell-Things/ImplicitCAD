-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

-- We'd like to parse openscad code, with some improvements, for backwards compatability.

module Graphics.Implicit.ExtOpenScad.Default where

import Graphics.Implicit.Definitions
import Graphics.Implicit.ExtOpenScad.Definitions
import Graphics.Implicit.ExtOpenScad.Util.OVal
import Graphics.Implicit.ExtOpenScad.Primitives
import Data.Map (fromList)

defaultObjects :: VarLookup -- = Map String OVal
defaultObjects = fromList $
    defaultConstants
    ++ defaultFunctions
    ++ defaultFunctions2
    ++ defaultFunctionsSpecial
    ++ defaultModules
    ++ defaultPolymorphicFunctions

-- Missing standard ones:
-- rand, lookup,

defaultConstants :: [([Char], OVal)]
defaultConstants = map (\(a,b) -> (a, toOObj (b::ℝ) ))
    [("pi", pi)]

defaultFunctions :: [([Char], OVal)]
defaultFunctions = map (\(a,b) -> (a, toOObj ( b :: ℝ -> ℝ)))
    [
        ("sin",   sin),
        ("cos",   cos),
        ("tan",   tan),
        ("asin",  asin),
        ("acos",  acos),
        ("atan",  atan),
        ("sinh",  sinh),
        ("cosh",  cosh),
        ("tanh",  tanh),
        ("abs",   abs),
        ("sign",  signum),
        ("floor", fromInteger . floor ),
        ("ceil",  fromInteger . ceiling ),
        ("round", fromInteger . round ),
        ("exp",   exp),
        ("ln",    log),
        ("log",   log),
        ("sign",  signum),
        ("sqrt",  sqrt)
    ]

defaultFunctions2 :: [([Char], OVal)]
defaultFunctions2 = map (\(a,b) -> (a, toOObj (b :: ℝ -> ℝ -> ℝ) ))
    [
        ("max", max),
        ("min", min),
        ("atan2", atan2),
        ("pow", (**))
    ]

defaultFunctionsSpecial :: [([Char], OVal)]
defaultFunctionsSpecial =
    [
        ("map", toOObj $ flip $
            (map :: (OVal -> OVal) -> [OVal] -> [OVal] )
        )
        
    ]


defaultModules :: [(String, OVal)]
defaultModules =
    map (\(a,b) -> (a, OModule b)) primitives



-- more complicated ones:

defaultPolymorphicFunctions :: [([Char], OVal)]
defaultPolymorphicFunctions =
    [
        ("+", sumtotal),
        ("sum", sumtotal),
        ("*", prod),
        ("prod", prod),
        ("/", divide),
        ("-", toOObj sub),
        ("%", toOObj omod),
        ("^", toOObj ((**) :: ℝ -> ℝ -> ℝ)),
        ("negate", toOObj negatefun),
        ("index", toOObj index),
        ("splice", toOObj osplice),
        ("<", toOObj  ((<) :: ℝ -> ℝ -> Bool) ),
        (">", toOObj  ((>) :: ℝ -> ℝ -> Bool) ),
        (">=", toOObj ((>=) :: ℝ -> ℝ -> Bool) ),
        ("<=", toOObj ((<=) :: ℝ -> ℝ -> Bool) ),
        ("==", toOObj ((==) :: OVal -> OVal -> Bool) ),
        ("!=", toOObj ((/=) :: OVal -> OVal -> Bool) ),
        ("?", toOObj ( ternary :: Bool -> OVal -> OVal -> OVal) ),
        ("&&", toOObj (&&) ),
        ("||", toOObj (||) ),
        ("!", toOObj not ),
        ("list_gen", toOObj list_gen),
        ("++", concatenate),
        ("len", toOObj olength),
        ("str", toOObj (show :: OVal -> String))
    ] where

        -- Some key functions are written as OVals in optimizations attempts

        prod = OFunc $ \x -> case x of
            (OList (y:ys)) -> foldl mult y ys
            (OList [])     -> ONum 1
            _              -> OError ["Product takes a list"]

        mult (ONum a)  (ONum b)  = ONum  (a*b)
        mult (ONum a)  (OList b) = OList (map (mult (ONum a)) b)
        mult (OList a) (ONum b)  = OList (map (mult (ONum b)) a)
        mult a         b         = errorAsAppropriate "multiply" a b

        divide = OFunc $ \x -> case x of
            (ONum a) -> OFunc $ \y -> case y of
                (ONum b) -> ONum (a/b)
                b        -> errorAsAppropriate "divide" (ONum a) b
            a -> OFunc $ \y -> case y of
                b -> div' a b

        div' (ONum a)  (ONum b) = ONum  (a/b)
        div' (OList a) (ONum b) = OList (map (\x -> div' x (ONum b)) a)
        div' a         b        = errorAsAppropriate "divide" a b

        omod (ONum a) (ONum b) = ONum $ fromInteger $ mod (floor a) (floor b)
        omod a        b        = errorAsAppropriate "modulo" a b

        append (OList   a) (OList   b) = OList   $ a++b
        append (OString a) (OString b) = OString $ a++b
        append a           b           = errorAsAppropriate "append" a b

        concatenate = OFunc $ \x -> case x of
            (OList (y:ys)) -> foldl append y ys
            (OList [])     -> OList []
            _              -> OError ["concat takes a list"]

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
            let n = floor ind
            in if n < length l then l !! n else OError ["List accessd out of bounds"]
        index (OString s) (ONum ind) =
            let n = floor ind
            in if n < length s then OString [s !! n] else OError ["List accessd out of bounds"]
        index a b = errorAsAppropriate "index" a b

        osplice (OList  list) (ONum a) (    ONum b    ) =
            OList   $ splice list (floor a) (floor b)
        osplice (OString str) (ONum a) (    ONum b    ) =
            OString $ splice str  (floor a) (floor b)
        osplice (OList  list) (OUndefined) (ONum b    ) =
            OList   $ splice list 0 (floor b)
        osplice (OString str) (OUndefined) (ONum b    ) =
            OString $ splice str  0 (floor b)
        osplice (OList  list) (ONum a) (    OUndefined) =
            OList   $ splice list (floor a) (length list + 1)
        osplice (OString str) (ONum a) (    OUndefined) =
            OString $ splice str  (floor a) (length str  + 1)
        osplice (OList  list) (OUndefined) (OUndefined) =
            OList   $ splice list 0 (length list + 1)
        osplice (OString str) (OUndefined) (OUndefined) =
            OString $ splice str  0 (length str  + 1)
        osplice _ _ _ = OUndefined

        splice :: [a] -> Int -> Int -> [a]
        splice [] _ _     = []
        splice (l@(x:xs)) a b
            |    a < 0  =    splice l   (a+n)  b
            |    b < 0  =    splice l    a    (b+n)
            |    a > 0  =    splice xs  (a-1) (b-1)
            |    b > 0  = x:(splice xs   a    (b-1) )
            | otherwise = []
                    where n = length l

        errorAsAppropriate _   err@(OError _)   _ = err
        errorAsAppropriate _   _   err@(OError _) = err
        errorAsAppropriate name a b = OError
            ["Can't " ++ name ++ " objects of types " ++ oTypeStr a ++ " and " ++ oTypeStr b ++ "."]

        list_gen :: [ℝ] -> Maybe [ℝ]
        list_gen [a,b] = Just [fromInteger (ceiling a).. fromInteger (floor b)]
        list_gen [a, b, c] =
            let
                nr = (c-a)/b
                n  = fromInteger (floor nr)
            in if nr - n > 0
            then Just
                [fromInteger (ceiling a), fromInteger (ceiling (a+b)).. fromInteger (floor (c - b*(nr -n)))]
            else Just
                [fromInteger (ceiling a), fromInteger (ceiling (a+b)).. fromInteger (floor c)]
        list_gen _ = Nothing

        ternary True a _ = a
        ternary False _ b = b

        olength (OString s) = ONum $ fromIntegral $ length s
        olength (OList s)   = ONum $ fromIntegral $ length s
        olength a           = OError ["Can't take length of a " ++ oTypeStr a ++ "."]


