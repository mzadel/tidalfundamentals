
module QueryDiagrams (example) where

import Shared (linearDiagramVerticalPadding)
import qualified LinearDiagrams as Lin (diagramShowCharValue,arcDiagram,queryDiagram)
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import qualified Sound.Tidal.Context as T

example :: T.Pattern Char -> T.Arc -> (T.Event Char -> Int) -> Diagram B
example pat arctoquery colourCharsFunc =
    vsep linearDiagramVerticalPadding [
        Lin.diagramShowCharValue pat 1 1 colourCharsFunc
        ,Lin.arcDiagram [arctoquery]
        ,Lin.queryDiagram (T.queryArc pat arctoquery) colourCharsFunc
        ]

