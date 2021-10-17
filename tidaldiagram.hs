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
        tickMarkPositions = map (/ numParts) [0..(numParts-1)] :: [Double]
        tickMarks = map tickMark tickMarkPositions

tickMark :: Double -> Diagram B
tickMark tickPos = mark
    where
        topPoint = p2 (0, theRadius)
        tickMarkSize = 0.1 * theRadius
        lineSeg = vrule tickMarkSize # moveTo topPoint
        mark = rotateBy tickPos $ lineSeg

outputScaling = 1000 :: Double

outputDiagram = circleWithTicks

main :: IO ()
main = mainWith (outputDiagram # scale outputScaling)

