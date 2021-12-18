
module LinearDiagrams.Shared (tickMark,tickMarkLabel,boxGeometry,labelGeometry,charToString) where

import Shared
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

tickMark :: Rational -> Diagram B
tickMark tickLoc = mark
    where
        tickMarkSize = 0.016
        point = p2 (fromRational tickLoc, 0)
        mark = vrule tickMarkSize # moveTo point

tickMarkLabel :: Rational -> Diagram B
tickMarkLabel tickLoc = label
    where
        labelPoint = p2 (fromRational tickLoc, 0)
        labelText = ratioToString tickLoc
        label = text labelText # fontSize tickMarkLabelSize # alignB # moveTo labelPoint

boxGeometry :: Rational -> Rational -> Diagram B
boxGeometry startLoc stopLoc = rect (fromRational $ stopLoc-startLoc) eventWidth # alignL # moveTo ((fromRational $ startLoc) ^& 0)

labelGeometry :: String -> Rational -> Diagram B
labelGeometry labelString boxStartLoc = label
    where
        label = alignedText 0 0.5 labelString # fontSize eventLabelSize # moveTo labelPoint
        labelPoint = (fromRational (boxStartLoc + eventLabelInset)) ^& 0

charToString :: Char -> String
charToString c = [c]

