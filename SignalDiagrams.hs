
module SignalDiagrams where

import Shared (linearDiagramVerticalPadding,curveValueAtTime,showDoubleTruncated,showValueMapTruncatedDouble,lineOfText)
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import qualified PatternExpressions as PE
import qualified LinearDiagrams.LinearDiagrams as Lin
import qualified LinearDiagrams.Arc as A
import qualified LinearDiagrams.Curve as C
import qualified Sound.Tidal.Context as T

sigEvaluatesAtMiddle :: Diagram B
sigEvaluatesAtMiddle =
    vsep linearDiagramVerticalPadding [
        (C.curveDiagramLabeledPoint thelocation labeltext
        <> C.curveDiagram PE.sigEvaluatesAtMiddleFunctionExpr 10)
        ,A.arcDiagram [PE.sigEvaluatesAtMiddleArcExpr]
        ]
    where
        T.Arc arcstart arcstop = PE.sigEvaluatesAtMiddleArcExpr
        arcmidpoint = (arcstop + arcstart) / 2.0
        thelocation :: P2 Double
        thelocation = (fromRational arcmidpoint) ^& valueatlocation
        valueatlocation :: Double
        valueatlocation = curveValueAtTime PE.sigEvaluatesAtMiddleFunctionExpr arcmidpoint
        labeltext = show valueatlocation

curvePointFromArc :: T.Pattern Double -> T.Arc -> Diagram B
curvePointFromArc pat thearc = C.curveDiagramLabeledPoint pt (showDoubleTruncated yval)
    where
        arcmidpoint :: T.Arc -> T.Time
        arcmidpoint (T.Arc arcstart arcstop) = (arcstop + arcstart) / 2
        t = arcmidpoint thearc
        yval = curveValueAtTime pat t
        pt = (fromRational t) ^& yval

sigToSetPanning :: Diagram B
sigToSetPanning =
    vsep linearDiagramVerticalPadding [
        (mconcat labeledpoints
        <> C.curveDiagram PE.sigToSetPanningFunctionExpr 1)
        ,strut (unitY * 0.01)
        ,lineOfText PE.sigToSetPanningSegmentedCurveStringExpr
        ,Lin.diagramFromWholes showDoubleTruncated PE.sigToSetPanningSegmentedCurveExpr 1
        ,strut (unitY * 0.01)
        ,lineOfText PE.sigToSetPanningStringExpr
        ,Lin.diagramFromWholes showValueMapTruncatedDouble PE.sigToSetPanningExpr 1
        ]
    where
        segmentevents = T.queryArc PE.sigToSetPanningSegmentedCurveExpr (T.Arc 0 1)
        arcs = map T.eventPart segmentevents
        labeledpoints = map (curvePointFromArc PE.sigToSetPanningFunctionExpr) arcs

