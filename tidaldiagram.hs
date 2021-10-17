{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

radiusOfUnitCircumfrenceCircle = 1.0 / (2.0 * pi) :: Double
theRadius = radiusOfUnitCircumfrenceCircle

{--
Verify that radiusOfUnitCircumfrenceCircle gives us a circle of circumfrence
1.0:

> c = circle radiusOfUnitCircumfrenceCircle :: Trail V2 Double
> stdArcLength c
1.0001402927134473
--}

circleWithTicks :: Diagram B
circleWithTicks = circle theRadius <> mconcat tickMarks <> mconcat tickMarkLabels
    where
        numParts = 7
        tickMarkLocations = map (/ numParts) [0..(numParts-1)] :: [Rational]
        tickMarks = map tickMark tickMarkLocations
        tickMarkLabels = map (tickMarkLabel 0.05) tickMarkLocations

tickMark :: Rational -> Diagram B
tickMark tickLoc = mark
    where
        tickMarkSize = 0.1 * theRadius
        topPoint = p2 (0, theRadius)
        lineSeg = vrule tickMarkSize # moveTo topPoint
        rotAmount = fromRational tickLoc
        mark = rotateBy rotAmount lineSeg

tickMarkLabel :: Double -> Rational -> Diagram B
tickMarkLabel extraRadius tickLoc = label
    where
        labelPoint = p2 (0, theRadius + extraRadius)
        rotAmount = fromRational tickLoc
        labelText = show tickLoc
        label = text labelText # fontSize (local 0.015) # moveTo (rotateBy rotAmount $ labelPoint)

patternEvents :: Diagram B
patternEvents = w
    where
        d :: Direction V2 Double
        d = rotateBy (1/4) xDir
        w = annularWedge (theRadius*1.1) (theRadius*0.9) d a # fc red # lw none
        a :: Angle Double
        a = (4 * tau / 7 - tau / 4) @@ rad

combined = patternEvents <> circleWithTicks

outputScaling = 1000 :: Double

outputDiagram = combined

main :: IO ()
main = mainWith (outputDiagram # frame 0.05 # scale outputScaling)

