-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright (C) 2016, Kelvin Cookshaw (kelvin@cookshaw.com)
-- Copyright 2014-2019, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

import Prelude (IO, String, Int, Either(Left, Right), return, show, ($), otherwise, (==), (-), (<>), mod, concat, error)
import Criterion.Main (Benchmark, bgroup, defaultMain, bench, env, whnf)
import Graphics.Implicit.ExtOpenScad.Definitions (Expr, StatementI)
import Graphics.Implicit.ExtOpenScad.Parser.Expr (expr0)
import Graphics.Implicit.ExtOpenScad.Parser.Statement (parseProgram)
import Text.ParserCombinators.Parsec (parse)
import Text.Printf (printf)

lineComment :: Int -> String
lineComment width = "//" <> ['x' | _ <- [1..width]] <> "\n"

lineComments :: Int -> String
lineComments n = concat [lineComment 80 | _ <- [1..n]]

blockComment :: Int -> Int -> String
blockComment lineCount width =
  "/*" <> concat [['x' | _ <- [1..width]] <> "\n" | _ <- [1..lineCount]] <> "*/"

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
echos n = concat ["echo(" <> show x <> ");" | x <- [1..n]]

ifs :: Int -> String
ifs n = concat ["if (true) {cube (10);} else {cube (20);}" | _ <- [1..n]]

fors :: Int -> String
fors n = concat ["for (i=[0:1:10]) {cube (i);}" | _ <- [1..n]]

moduleCalls :: Int -> String
moduleCalls n = concat ["moduleno" <> show x <> " (" <> show x <> ");" | x <- [1..n]]

moduleDeclarations :: Int -> String
moduleDeclarations n = concat ["module modulename(arg, arg2=10) { cube(arg2); }" | _ <- [1..n]]

ternary :: Int -> String
ternary n = concat ["true?1:" | _ <- [1..n]] <> "2"

lets :: Int -> String
lets n = concat ["let (a=1) " | _ <- [1..n]] <> " a"

intList :: Int -> String
intList n = "[" <> concat [show i <> "," | i <- [1..n]] <> "0]"

intParList :: Int -> String
intParList n = "(" <> concat [show i <> "," | i <- [1..n]] <> "0)"

intPosNegList :: Int -> String
intPosNegList n = "[" <> concat [posOrNeg i <> show i <> "," | i <- [1..n]] <> "0]"
  where
    posOrNeg :: Int -> String
    posOrNeg x = if x `mod` 2 == 1
                 then "+"
                 else "-"

parExpr :: Int -> String
parExpr n = concat ["(a+" <> show i <> "+" | i <- [0..n]] <> "0)" <> concat ["+" <> show i <> ")" | i <- [1..n]]

genList :: Int -> String
genList n = concat ["[1:1:" <> show i <> "] ++ " | i <- [1..n]] <> "0"

stringList :: Int -> String
stringList n = "[" <> concat ["\"" <> show i <> "\", " | i <- [1..n]] <> " \"something\"]"

boolList :: Int -> String
boolList n = "[" <> concat [trueOrFalse i <> "," | i <- [1..n]] <> "false]"
  where
    trueOrFalse :: Int -> String
    trueOrFalse x = if x `mod` 2 == 1
                    then "true"
                    else "false"

undefinedList :: Int -> String
undefinedList n = "[" <> concat ["undef, " | _ <- [1..n]] <> "undef]"

deepArithmetic :: Int -> String
deepArithmetic n
  | n == 0 = "1"
  | otherwise = printf "%s + %s * (%s - %s)" d d d d
                where
                  d = deepArithmetic (n - 1)

parseExpr :: String -> Expr
parseExpr s = case parse expr0 "src" s of
               Left err -> error (show err)
               Right e -> e

parseStatements :: String -> [StatementI]
parseStatements s = case parseProgram "noname" s of
                     Left err -> error (show err)
                     Right e -> e

run :: String -> (String -> a) -> String -> Benchmark
run name func input =
  env (return input) $ \s ->
  bench name $ whnf func s

main :: IO ()
main =
  defaultMain
  [ bgroup "lexer" [
      bgroup "comments"
      [ run "line" parseStatements (lineComments 5000)
      , run "block" parseStatements (blockComments 10 500)
      ]
    ]
  , bgroup "statement" [
      run "throwAway" parseStatements (throwAway 500)
      , bgroup "includes"
        [ run "include" parseStatements (include 5000)
        , run "use" parseStatements (use 5000)
        ]
      , run "assignments" parseStatements (assignments 50)
      , run "function declarations" parseStatements (functionDeclarations 100)
      , run "echos" parseStatements (echos 1000)
      , run "ifs" parseStatements (ifs 250)
      , run "fors" parseStatements (fors 50)
      , run "module calls" parseStatements (moduleCalls 500)
      , run "module declarations" parseStatements (moduleDeclarations 500)
      ]
  , bgroup "expression" [
        run "ternary operators" parseExpr (ternary 500)
      , run "let statements" parseExpr (lets 3)
      , run "int list" parseExpr (intList 100)
      , run "parenthesized int list" parseExpr (intParList 100)
      , run "parenthesized expression" parseExpr (parExpr 2)
      , run "generated list" parseExpr (genList 50)
      , run "list of positive or negative integers" parseExpr (intPosNegList 100)
      , run "string list" parseExpr (stringList 100)
      , run "bool list" parseExpr (boolList 100)
      , run "undefined list" parseExpr (undefinedList 100)
      , run "deep arithmetic" parseExpr (deepArithmetic 3)
      ]
  ]
