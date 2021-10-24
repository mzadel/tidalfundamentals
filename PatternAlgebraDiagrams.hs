
module PatternAlgebraDiagrams where

import TidalPatternDiagram
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import qualified Sound.Tidal.Context as T

lineOfText :: String -> Diagram B
lineOfText texttoshow = (alignedText 0 0.5 texttoshow # fontSize eventLabelSize) <> (strut (0.0 ^& 0.03))

testPatternAlgebraDiagram :: Diagram B
testPatternAlgebraDiagram =
    vsep linearDiagramVerticalPadding [
        patternDiagramLinearWithDoubles (T.parseBP_E "1 2 3" :: T.Pattern Double) 1
        ,lineOfText "yadda"
        ,patternDiagramLinearWithDoubles (T.parseBP_E "1 2 3" :: T.Pattern Double) 1
        ,lineOfText "yadda"
        ,patternDiagramLinearWithDoubles (T.parseBP_E "1 2 3" :: T.Pattern Double) 1
        ]

