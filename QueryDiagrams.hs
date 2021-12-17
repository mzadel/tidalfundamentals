
module QueryDiagrams (example) where

import Shared (linearDiagramVerticalPadding)
import qualified LinearDiagrams as Lin (diagramShowCharValue,arcDiagram,queryDiagram)
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import qualified Sound.Tidal.Context as T

example :: (T.Event Char -> Int) -> Diagram B
example colourCharsFunc =
    vsep linearDiagramVerticalPadding [
        Lin.diagramShowCharValue pat 1 1 colourCharsFunc
        ,Lin.arcDiagram [arctoquery]
        ,Lin.queryDiagram (T.queryArc pat arctoquery) colourCharsFunc
        ]
    where
        pat = T.fromMaybes [Just 'a',Nothing,Just 'c']
        arctoquery = T.Arc 0.25 0.75

