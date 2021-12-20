
module PatternAlgebraDiagrams where

import Shared
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import qualified PatternExpressions as PE
import qualified LinearDiagrams.LinearDiagrams as Lin
import qualified Sound.Tidal.Context as T

patternAlgebraDiagram :: (Show a) => T.Pattern a -> String -> T.Pattern a -> T.Pattern a -> Diagram B
patternAlgebraDiagram leftpat operatorString rightpat combinedpat =
    vsep linearDiagramVerticalPadding [
        Lin.diagramFromWholes show (leftpat) 1
        ,lineOfText operatorString
        ,Lin.diagramFromWholes show (rightpat) 1
        ,lineOfText "=="
        ,Lin.diagramFromWholes show combinedpat 1
        ]

leftPlusExample1 :: Diagram B
leftPlusExample1 = patternAlgebraDiagram PE.leftPlusExample1LeftExpr PE.leftPlusExample1OperatorStringExpr PE.leftPlusExample1RightExpr PE.leftPlusExample1Expr
leftPlusExample2 :: Diagram B
leftPlusExample2 = patternAlgebraDiagram PE.leftPlusExample2LeftExpr PE.leftPlusExample2OperatorStringExpr PE.leftPlusExample2RightExpr PE.leftPlusExample2Expr
leftPlusExample3 :: Diagram B
leftPlusExample3 = patternAlgebraDiagram PE.leftPlusExample3LeftExpr PE.leftPlusExample3OperatorStringExpr PE.leftPlusExample3RightExpr PE.leftPlusExample3Expr

rightPlusExample1 :: Diagram B
rightPlusExample1 = patternAlgebraDiagram PE.rightPlusExample1LeftExpr PE.rightPlusExample1OperatorStringExpr PE.rightPlusExample1RightExpr PE.rightPlusExample1Expr
rightPlusExample2 :: Diagram B
rightPlusExample2 = patternAlgebraDiagram PE.rightPlusExample2LeftExpr PE.rightPlusExample2OperatorStringExpr PE.rightPlusExample2RightExpr PE.rightPlusExample2Expr
rightPlusExample3 :: Diagram B
rightPlusExample3 = patternAlgebraDiagram PE.rightPlusExample3LeftExpr PE.rightPlusExample3OperatorStringExpr PE.rightPlusExample3RightExpr PE.rightPlusExample3Expr

bothPlusExample1 :: Diagram B
bothPlusExample1 = patternAlgebraDiagram PE.bothPlusExample1LeftExpr PE.bothPlusExample1OperatorStringExpr PE.bothPlusExample1RightExpr PE.bothPlusExample1Expr
bothPlusExample2 :: Diagram B
bothPlusExample2 = patternAlgebraDiagram PE.bothPlusExample2LeftExpr PE.bothPlusExample2OperatorStringExpr PE.bothPlusExample2RightExpr PE.bothPlusExample2Expr
bothPlusExample3 :: Diagram B
bothPlusExample3 = patternAlgebraDiagram PE.bothPlusExample3LeftExpr PE.bothPlusExample3OperatorStringExpr PE.bothPlusExample3RightExpr PE.bothPlusExample3Expr

justPlusExample1 :: Diagram B
justPlusExample1 = patternAlgebraDiagram PE.justPlusExample1LeftExpr PE.justPlusExample1OperatorStringExpr PE.justPlusExample1RightExpr PE.justPlusExample1Expr

valueAlgebraMapDiagram :: Diagram B
valueAlgebraMapDiagram =
    vsep linearDiagramVerticalPadding [
        Lin.diagramFromWholes showValueMap (PE.valueAlgebraMapDiagramLeftExpr :: T.ControlPattern) 3
        ,lineOfText PE.valueAlgebraMapDiagramOperatorStringExpr
        ,Lin.diagramFromWholes showValueMap (PE.valueAlgebraMapDiagramRightExpr :: T.ControlPattern) 3
        ,lineOfText "=="
        ,Lin.diagramFromWholes showValueMap (PE.valueAlgebraMapDiagramExpr :: T.ControlPattern) 3
        ]

