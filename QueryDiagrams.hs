
module QueryDiagrams (example,continuousPatternExample) where

import Shared (linearDiagramVerticalPadding,lineOfText)
import qualified LinearDiagrams.LinearDiagrams as Lin (diagramShowCharValue)
import qualified LinearDiagrams.Arc as Lin (arcDiagram)
import qualified LinearDiagrams.Query as Q (queryDiagramChar,queryDiagramDouble)
import qualified LinearDiagrams.Curve as C (curveDiagram)
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

