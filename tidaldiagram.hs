{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

radiusOfUnitCircumfrenceCircle = 1.0 / (2.0 * pi) :: Double

{--
Verify that radiusOfUnitCircumfrenceCircle gives us a circle of circumfrence
1.0:

> c = circle radiusOfUnitCircumfrenceCircle :: Trail V2 Double
> stdArcLength c
1.0001402927134473
--}

circleWithTicks :: Diagram B
circleWithTicks = circle radiusOfUnitCircumfrenceCircle <> mconcat tickMarks
    where
        numParts = 7
        tickMarkSize = 0.1 * radiusOfUnitCircumfrenceCircle
        topPoint = p2 (0, radiusOfUnitCircumfrenceCircle)
        lineSeg = vrule tickMarkSize # moveTo topPoint
        tickMarkPositions = map (/ numParts) [0..(numParts-1)] :: [Double]
        tickMarks = [ rotateBy r $ lineSeg | r <- tickMarkPositions]

myCircle :: Diagram B
myCircle = circle radiusOfUnitCircumfrenceCircle # fc green

outputScaling = 1000 :: Double

outputDiagram = circleWithTicks

main :: IO ()
main = mainWith (outputDiagram # scale outputScaling)

