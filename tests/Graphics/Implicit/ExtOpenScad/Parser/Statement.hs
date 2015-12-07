import Graphics.Implicit.ExtOpenScad.Parser.Statement
import Graphics.Implicit.ExtOpenScad.Definitions
import Data.Either
import Test.HUnit

main = runTestTT $ TestList [
  TestCase $ assertBool "Failure to separate two cylinder objects with a semicolon should be an error." $
    isLeft (parseProgram "src" "difference(){ cylinder(r=5,h=20) cylinder(r=2,h=20); }"),
      
  parseProgram "src" "difference(){ cylinder(r=5,h=20); cylinder(r=2,h=20); }"
    ~?= Right [
      StatementI 1 (ModuleCall "difference" [] [
        StatementI 1 (ModuleCall "cylinder" [(Just "r",LitE (ONum 5.0)),(Just "h",LitE (ONum 20.0))] []),
        StatementI 1 (ModuleCall "cylinder" [(Just "r",LitE (ONum 2.0)),(Just "h",LitE (ONum 20.0))] [])])
        ]
    ]

