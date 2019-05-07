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
                 ++ assignments 1 -- to avoid empty file

blockComment :: Int -> Int -> String
blockComment lineCount width =
  "/*" ++ concat [['x' | _ <- [1..width]] ++ "\n" | _ <- [1..lineCount]] ++ "*/"

blockComments :: Int -> Int -> String
blockComments lineCount n = concat [blockComment lineCount 40 | _ <- [1..n]]
                            ++ assignments 1 -- to avoid empty file

assignments :: Int -> String
assignments n = concat ["x = (foo + bar);\n" | _ <- [1..n]]

intList :: Int -> String
intList n = "[" ++ concat [show i ++ "," | i <- [1..n]] ++ "0]"

parseExpr :: String -> Expr
parseExpr s = case parse expr0 "src" s of
               Left err -> error (show err)
               Right e -> e

parseStatements :: String -> [StatementI]
parseStatements s = case parseProgram s of
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
  , run "assignments" parseStatements (assignments 100)
  , run "int list" parseExpr (intList 1000)
  , run "deep arithmetic" parseExpr (deepArithmetic 3)
  ]
