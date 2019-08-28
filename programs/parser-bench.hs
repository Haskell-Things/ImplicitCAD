import Prelude (IO, String, Int, Either(Left, Right), return, show, ($), otherwise, (==), (-), (++), concat, error)
import Criterion.Main (Benchmark, bgroup, defaultMain, bench, env, whnf)
import Graphics.Implicit.ExtOpenScad.Definitions (Expr, StatementI)
import Graphics.Implicit.ExtOpenScad.Parser.Expr (expr0)
import Graphics.Implicit.ExtOpenScad.Parser.Statement (parseProgram)
import Text.ParserCombinators.Parsec (parse)
import Text.Printf (printf)

lineComment :: Int -> String
lineComment width = "//" ++ ['x' | _ <- [1..width]] ++ "\n"

lineComments :: Int -> String
lineComments n = concat [lineComment 80 | _ <- [1..n]]

blockComment :: Int -> Int -> String
blockComment lineCount width =
  "/*" ++ concat [['x' | _ <- [1..width]] ++ "\n" | _ <- [1..lineCount]] ++ "*/"

blockComments :: Int -> Int -> String
blockComments lineCount n = concat [blockComment lineCount 40 | _ <- [1..n]]

throwAway :: Int -> String
throwAway n = concat ["%cube (10);*cube (10);" | _ <- [1..n]]

include :: Int -> String
include n = concat ["include <header.escad>;" | _ <- [1..n]]

use :: Int -> String
use n = concat ["use <header.escad>;" | _ <- [1..n]]

assignments :: Int -> String
assignments n = concat ["x = (foo + bar);\n" | _ <- [1..n]]

functionDeclarations :: Int -> String
functionDeclarations n = concat ["function functionname(arg, arg2) = sin(arg*arg2);" | _ <- [1..n]]

echos :: Int -> String
echos n = concat ["echo(" ++ show x ++ ");" | x <- [1..n]]

ifs :: Int -> String
ifs n = concat ["if (true) {cube (10);} else {cube (20);}" | _ <- [1..n]]

fors :: Int -> String
fors n = concat ["for (i=[0:1:10]) {cube (i);}" | _ <- [1..n]]

moduleDeclarations :: Int -> String
moduleDeclarations n = concat ["module modulename(arg, arg2=10) { cube(arg2); }" | _ <- [1..n]]


intList :: Int -> String
intList n = "[" ++ concat [show i ++ "," | i <- [1..n]] ++ "0]"

parseExpr :: String -> Expr
parseExpr s = case parse expr0 "src" s of
               Left err -> error (show err)
               Right e -> e

parseStatements :: String -> [StatementI]
parseStatements s = case parseProgram "noname" s of
                     Left err -> error (show err)
                     Right e -> e

deepArithmetic :: Int -> String
deepArithmetic n
  | n == 0 = "1"
  | otherwise = printf "%s + %s * (%s - %s)" d d d d
                where
                  d = deepArithmetic (n - 1)

run :: String -> (String -> a) -> String -> Benchmark
run name func input =
  env (return input) $ \s ->
  bench name $ whnf func s

main :: IO ()
main =
  defaultMain
  [ bgroup "comments"
    [ run "line" parseStatements (lineComments 5000)
    , run "block" parseStatements (blockComments 10 500)
    ]
  , run "throwAway" parseStatements (throwAway 500)
  , bgroup "includes"
    [ run "include" parseStatements (include 5000)
    , run "use" parseStatements (use 5000)
    ]
  , run "assignments" parseStatements (assignments 50)
  , run "function declarations" parseStatements (functionDeclarations 100)
  , run "echos" parseStatements (echos 1000)
  , run "ifs" parseStatements (ifs 250)
  , run "fors" parseStatements (fors 50)
  , run "module declarations" parseStatements (moduleDeclarations 500)
  , run "int list" parseExpr (intList 250)
  , run "deep arithmetic" parseExpr (deepArithmetic 3)
  ]
