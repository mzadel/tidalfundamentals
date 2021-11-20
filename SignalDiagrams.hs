
module SignalDiagrams where

import Shared (linearDiagramVerticalPadding,curveValueAtTime)
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import qualified PatternExpressions as PE
import qualified LinearDiagrams as Lin
import qualified Sound.Tidal.Context as T

sigEvaluatesAtMiddle :: Diagram B
sigEvaluatesAtMiddle =
    vsep linearDiagramVerticalPadding [
        (Lin.curveDiagramLabeledPoint thelocation labeltext
        <> Lin.curveDiagram PE.sigEvaluatesAtMiddleFunctionExpr 10)
        ,Lin.arcDiagram [PE.sigEvaluatesAtMiddleArcExpr]
        ]
    where
        T.Arc arcstart arcstop = PE.sigEvaluatesAtMiddleArcExpr
        arcmidpoint = (arcstop + arcstart) / 2.0
        thelocation :: P2 Double
        thelocation = (fromRational arcmidpoint) ^& valueatlocation
        valueatlocation :: Double
        valueatlocation = curveValueAtTime PE.sigEvaluatesAtMiddleFunctionExpr arcmidpoint
        labeltext = show valueatlocation

