
module PatternAlgebraDiagrams where

import Shared
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import qualified PatternExpressions as PE
import qualified LinearDiagrams as Lin
import qualified Sound.Tidal.Context as T

lineOfText :: String -> Diagram B
lineOfText texttoshow = (alignedText 0 0.5 texttoshow # fontSize eventLabelSize) <> (strut (0.0 ^& 0.03))

patternAlgebraDiagram :: (T.Pattern Double -> T.Pattern Double -> T.Pattern Double) -> String -> T.Pattern Double -> T.Pattern Double -> Diagram B
patternAlgebraDiagram operator operatorString pat1 pat2 =
    vsep linearDiagramVerticalPadding [
        Lin.diagramFromWholes (pat1) 1
        ,lineOfText operatorString
        ,Lin.diagramFromWholes (pat2) 1
        ,lineOfText "=="
        ,Lin.diagramFromWholes (operator pat1 pat2) 1
        ]

leftPlusExample1 :: Diagram B
leftPlusExample1 = patternAlgebraDiagram (T.|+) "|+" (T.parseBP_E "1 2 3") (T.parseBP_E "20 40")
leftPlusExample2 :: Diagram B
leftPlusExample2 = patternAlgebraDiagram (T.|+) "|+" (T.parseBP_E "1 2 3") (T.parseBP_E "20 40 60")
leftPlusExample3 :: Diagram B
leftPlusExample3 = patternAlgebraDiagram (T.|+) "|+" (T.parseBP_E "1 2 3") (T.parseBP_E "20 40 60 80")

rightPlusExample1 :: Diagram B
rightPlusExample1 = patternAlgebraDiagram PE.rightPlusExample1OperatorExpr PE.rightPlusExample1OperatorStringExpr PE.rightPlusExample1LeftExpr PE.rightPlusExample1RightExpr
rightPlusExample2 :: Diagram B
rightPlusExample2 = patternAlgebraDiagram PE.rightPlusExample2OperatorExpr PE.rightPlusExample2OperatorStringExpr PE.rightPlusExample2LeftExpr PE.rightPlusExample2RightExpr
rightPlusExample3 :: Diagram B
rightPlusExample3 = patternAlgebraDiagram PE.rightPlusExample3OperatorExpr PE.rightPlusExample3OperatorStringExpr PE.rightPlusExample3LeftExpr PE.rightPlusExample3RightExpr

bothPlusExample1 :: Diagram B
bothPlusExample1 = patternAlgebraDiagram PE.bothPlusExample1OperatorExpr PE.bothPlusExample1OperatorStringExpr PE.bothPlusExample1LeftExpr PE.bothPlusExample1RightExpr
bothPlusExample2 :: Diagram B
bothPlusExample2 = patternAlgebraDiagram PE.bothPlusExample2OperatorExpr PE.bothPlusExample2OperatorStringExpr PE.bothPlusExample2LeftExpr PE.bothPlusExample2RightExpr
bothPlusExample3 :: Diagram B
bothPlusExample3 = patternAlgebraDiagram PE.bothPlusExample3OperatorExpr PE.bothPlusExample3OperatorStringExpr PE.bothPlusExample3LeftExpr PE.bothPlusExample3RightExpr

justPlusExample1 :: Diagram B
justPlusExample1 = patternAlgebraDiagram (+) "+" (T.parseBP_E "1 2 3") (T.parseBP_E "20 40 60 80")

valueAlgebraMapDiagram :: Diagram B
valueAlgebraMapDiagram =
    vsep linearDiagramVerticalPadding [
        Lin.diagramFromWholes (T.s $ T.parseBP_E "bd sd hh") 3
        ,lineOfText "|+"
        ,Lin.diagramFromWholes (T.pan $ T.slow 3 $ T.parseBP_E "0.2 0.5 0.7") 3
        ,lineOfText "=="
        ,Lin.diagramFromWholes ((T.s $ T.parseBP_E "bd sd hh") T.|+ (T.pan $ T.slow 3 $ T.parseBP_E "0.2 0.5 0.7")) 3
        ]

