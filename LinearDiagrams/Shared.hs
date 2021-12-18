
module LinearDiagrams.Shared (boxGeometry,labelGeometry,charToString) where

import Shared
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

boxGeometry :: Rational -> Rational -> Diagram B
boxGeometry startLoc stopLoc = rect (fromRational $ stopLoc-startLoc) eventWidth # alignL # moveTo ((fromRational $ startLoc) ^& 0)

labelGeometry :: String -> Rational -> Diagram B
labelGeometry labelString boxStartLoc = label
    where
        label = alignedText 0 0.5 labelString # fontSize eventLabelSize # moveTo labelPoint
        labelPoint = (fromRational (boxStartLoc + eventLabelInset)) ^& 0

charToString :: Char -> String
charToString c = [c]

