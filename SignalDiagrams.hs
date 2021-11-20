
module SignalDiagrams where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import qualified PatternExpressions as PE
import qualified LinearDiagrams as Lin

sigEvaluatesAtMiddle :: Diagram B
sigEvaluatesAtMiddle = Lin.curveDiagram PE.sigEvaluatesAtMiddleFunctionExpr 10

