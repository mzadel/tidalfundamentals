
module Shared where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Data.Colour.Palette.ColorSet
import Data.Ratio

ratioToString :: Rational -> String
ratioToString r = case (denominator r) of
    1 -> show $ numerator r
    _ -> (show $ numerator r) ++ "/" ++ (show $ denominator r)

tickMarkLabelSize = local 0.02

eventWidth = 0.035
eventLabelSize = local 0.03
eventLabelInset = 0.02 :: Rational

linearDiagramVerticalPadding = 0.01

tickMarkLocations :: Rational -> Rational -> [Rational]
tickMarkLocations tickDivision endTickLoc = [0,tickDivision..endTickLoc]

style :: Brightness -> Int -> Diagram B -> Diagram B
style brightness colourindex = lw none $ fc $ d3Colors2 brightness colourindex

