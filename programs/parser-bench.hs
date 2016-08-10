import Prelude (IO, String, Char, Int, Either(Left, Right), return, show, ($), otherwise, (==), (-), (++), concat, error)
import Criterion.Main (Benchmark, bgroup, defaultMain, bench, env, whnf)
import Graphics.Implicit.ExtOpenScad.Definitions (Expr, StatementI)
import qualified Graphics.Implicit.ExtOpenScad.Parser.Expr as Orig (expr0)
import qualified Graphics.Implicit.ExtOpenScad.Parser.Statement as Orig (parseProgram)
import Graphics.Implicit.ExtOpenScad.Parser.AltExpr (altExpr)
import Graphics.Implicit.ExtOpenScad.Parser.AltStatement (altParseProgram)
import Graphics.Implicit.ExtOpenScad.Parser.Statement (parseProgram)
import Text.ParserCombinators.Parsec (parse, SourceName, ParseError, GenParser)
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

parseExpr :: (GenParser Char () Expr) -> String -> Expr
parseExpr exprParser s = case parse exprParser "src" s of
               Left err -> error (show err)
               Right e -> e

parseStatements :: (SourceName -> String -> Either ParseError [StatementI]) -> String -> [StatementI]
parseStatements parseProgram s = case parseProgram "noname" s of
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
  defaultMain $
  [ bgroup "orig"
    [ bgroup "comments"
      [ run "line" (parseStatements Orig.parseProgram) (lineComments 5000)
      , run "block" (parseStatements Orig.parseProgram) (blockComments 10 500)
      ]
    , run "assignments" (parseStatements Orig.parseProgram) (assignments 100)
    , run "int list" (parseExpr Orig.expr0) (intList 1000)
    , run "deep arithmetic" (parseExpr Orig.expr0) (deepArithmetic 3)
    ]
  , bgroup "alt"
    [ bgroup "comments"
      [ run "line" (parseStatements altParseProgram) (lineComments 5000)
      , run "block" (parseStatements altParseProgram) (blockComments 10 500)
      ]
    , run "assignments" (parseStatements altParseProgram) (assignments 100)
    , run "int list" (parseExpr altExpr) (intList 1000)
    , run "deep arithmetic" (parseExpr altExpr) (deepArithmetic 3)
    ]
  ]
