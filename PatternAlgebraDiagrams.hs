
module PatternAlgebraDiagrams where

import Shared
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
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
rightPlusExample1 = patternAlgebraDiagram (T.+|) "+|" (T.parseBP_E "1 2 3") (T.parseBP_E "20 40")
rightPlusExample2 :: Diagram B
rightPlusExample2 = patternAlgebraDiagram (T.+|) "+|" (T.parseBP_E "1 2 3") (T.parseBP_E "20 40 60")
rightPlusExample3 :: Diagram B
rightPlusExample3 = patternAlgebraDiagram (T.+|) "+|" (T.parseBP_E "1 2 3") (T.parseBP_E "20 40 60 80")

bothPlusExample1 :: Diagram B
bothPlusExample1 = patternAlgebraDiagram (T.|+|) "|+|" (T.parseBP_E "1 2 3") (T.parseBP_E "20 40")
bothPlusExample2 :: Diagram B
bothPlusExample2 = patternAlgebraDiagram (T.|+|) "|+|" (T.parseBP_E "1 2 3") (T.parseBP_E "20 40 60")
bothPlusExample3 :: Diagram B
bothPlusExample3 = patternAlgebraDiagram (T.|+|) "|+|" (T.parseBP_E "1 2 3") (T.parseBP_E "20 40 60 80")

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

