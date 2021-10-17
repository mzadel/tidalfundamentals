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
circleWithTicks = circle theRadius <> mconcat tickMarks
    where
        numParts = 7
        tickMarkPositions = map (/ numParts) [0..(numParts-1)] :: [Rational]
        tickMarks = map tickMark tickMarkPositions

tickMark :: Rational -> Diagram B
tickMark tickPos = mark
    where
        tickMarkSize = 0.1 * theRadius
        topPoint = p2 (0, theRadius)
        labelPoint = p2 (0, theRadius + 2 * tickMarkSize)
        lineSeg = vrule tickMarkSize # moveTo topPoint
        rotAmount = fromRational tickPos
        mark = rotateBy rotAmount lineSeg <> label
        labelText = show tickPos
        label = text labelText # fontSize (local 0.015) # moveTo (rotateBy rotAmount $ labelPoint)

outputScaling = 1000 :: Double

outputDiagram = circleWithTicks

main :: IO ()
main = mainWith (outputDiagram # frame 0.05 # scale outputScaling)

