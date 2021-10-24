
module PatternAlgebraDiagrams where

import TidalPatternDiagram
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import qualified Sound.Tidal.Context as T

lineOfText :: String -> Diagram B
lineOfText texttoshow = (alignedText 0 0.5 texttoshow # fontSize eventLabelSize) <> (strut (0.0 ^& 0.03))

patternAlgebraDiagram :: (T.Pattern Double -> T.Pattern Double -> T.Pattern Double) -> String -> T.Pattern Double -> T.Pattern Double -> Diagram B
patternAlgebraDiagram operator operatorString pat1 pat2 =
    vsep linearDiagramVerticalPadding [
        patternDiagramLinearWithDoubles (pat1) 1
        ,lineOfText operatorString
        ,patternDiagramLinearWithDoubles (pat2) 1
        ,lineOfText "=="
        ,patternDiagramLinearWithDoubles (operator pat1 pat2) 1
        ]

leftPlusExample1 :: Diagram B
leftPlusExample1 = patternAlgebraDiagram (T.|+) "|+" (T.parseBP_E "1 2 3") (T.parseBP_E "20 40")
leftPlusExample2 :: Diagram B
leftPlusExample2 = patternAlgebraDiagram (T.|+) "|+" (T.parseBP_E "1 2 3") (T.parseBP_E "20 40 60")

