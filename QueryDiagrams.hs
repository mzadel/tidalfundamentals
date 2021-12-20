
module QueryDiagrams (example,continuousPatternExample,continuousPatternExampleShowPoint) where

import Shared (linearDiagramVerticalPadding,arcMidpoint,showDoubleTruncated,lineOfText)
import qualified LinearDiagrams.LinearDiagrams as Lin (diagramShowCharValue)
import qualified LinearDiagrams.Arc as Lin (arcDiagram)
import qualified LinearDiagrams.Query as Q (queryDiagramChar,queryDiagramDouble)
import qualified LinearDiagrams.Curve as C (curveDiagram,curveDiagramLabeledPoint)
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import qualified Sound.Tidal.Context as T

example :: T.Pattern Char -> T.Arc -> (T.Event Char -> Int) -> Diagram B
example pat arctoquery colourCharsFunc =
    vsep linearDiagramVerticalPadding [
        Lin.diagramShowCharValue pat 1 1 colourCharsFunc
        ,lineOfText "Arc to query:"
        ,Lin.arcDiagram [arctoquery]
        ,lineOfText "Result:"
        ,Q.queryDiagramChar (T.queryArc pat arctoquery) colourCharsFunc
        ]

continuousPatternExample :: T.Pattern Double -> T.Arc -> Diagram B
continuousPatternExample ctspat arctoquery =
    vsep linearDiagramVerticalPadding [
        C.curveDiagram ctspat 1
        ,lineOfText "Arc to query:"
        ,Lin.arcDiagram [arctoquery]
        ,lineOfText "Result:"
        ,Q.queryDiagramDouble (T.queryArc ctspat arctoquery)
        ]

continuousPatternExampleShowPoint :: T.Pattern Double -> T.Arc -> Diagram B
continuousPatternExampleShowPoint ctspat arctoquery =
    vsep linearDiagramVerticalPadding [
        C.curveDiagramLabeledPoint showDoubleTruncated ctspat (arcMidpoint arctoquery) <> C.curveDiagram ctspat 1
        ,lineOfText "Arc to query:"
        ,Lin.arcDiagram [arctoquery]
        ,lineOfText "Result:"
        ,Q.queryDiagramDouble (T.queryArc ctspat arctoquery)
        ]

